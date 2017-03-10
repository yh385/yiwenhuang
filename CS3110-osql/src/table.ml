open Ast 
open VecAst
open Vector 
open Marshal

module type TableType = sig
  type t
  
  val db_dir : string
  val table_dir : string
  val table_postfix : string
  val create_empty : string -> col_attri list -> string -> t
  val create_as    : string -> col_attri list -> t -> t 
  val drop_table   : string -> unit 
  val truncate_tbl : t -> unit 
  val insert_into : t -> (string * value) list -> unit 

  val select : sel_dist -> (col_op * sub_name) list -> t * (alias list) 
               -> where_cond -> group_cond -> hav_cond -> order_cond -> limit_num -> t
  val from   : from_obj -> t * (alias list)
  (* val remove : 'key list -> 'row, 'key t -> 'row, 'key t *)
  val update : t -> (string * value) list -> where_cond -> unit
  val delete_from: t -> where_cond -> unit 
  val add_col : col_name -> string -> t -> t
  val drop_col : string -> t -> t 
  val exist : string -> bool 
  val print_table : t -> unit 
  val raw_data : t -> value array array
  val get_vec  : t -> vector
  (* [store_tbl tbl] will serialize [tbl] to a binary file. *)
  val store_tbl : t -> unit 
  (* [load_tbl file_name] will deserialize a table structure from [file_name]*)
  val load_tbl  : string -> t
  val test_print : unit -> unit 
end 
(*   | Select of sel_dist * (col_op * sub_name) list * from_obj * where_cond 
                                       * group_cond * hav_cond * order_cond * limit_num *)

module Table: TableType  = struct 
  type t = {table_name: string;
            col_attribute: (string * col_info) array;
            (* index *)
            indexed_col  : string list; 
            data_vec: vector
            }
   and col_info = {dtype     : val_type;
                   not_null  : bool;
                   default   : value;
                   auto_incr : bool  }  
    (*------------------------------------------------------*)              
    (*                   Constants                          *)
    (*------------------------------------------------------*)
    let db_dir = "database"
    let table_dir = "."^Filename.dir_sep^
                    db_dir^Filename.dir_sep^"tables"^Filename.dir_sep
    let table_postfix = ".mary"

    (*------------------------------------------------------*)              
    (*                   Helper Stuff                       *)
    (*------------------------------------------------------*)

    let string_to_type v = 
      match v with 
      | "date"     -> `Date    | "datetime"  -> `DateTime  
      | "time"     -> `Time    | "int"       -> `Int       
      | "float"    -> `Float   | "bool"      -> `Bool      
      | "string"   -> `String  | "char"      -> `Char      
      | "null"     -> `Null    | _ -> failwith ("Unknown type "^".")
    (* TODO: more flexible matching rule *)
    let typematch v t = 
      match v,t with
      | VDate   _ , `Date   | VDateTime _ , `DateTime  
      | VTime   _ , `Time   | VInt      _ , `Int       
      | VFloat  _ , `Float  | VBool     _ , `Bool      
      | VString _ , `String | VChar     _ , `Char      
      | VNull     , _  -> true 
      | _, _           -> false  

    (* [typeof v] samples a value and gets its type. *)
    let typeof v = 
      match v with    
      | VDate     _ -> `Date    | VDateTime _ -> `DateTime  
      | VTime     _ -> `Time    | VInt      _ -> `Int       
      | VFloat    _ -> `Float   | VBool     _ -> `Bool      
      | VString   _ -> `String  | VChar     _ -> `Char      
      | VNull       -> `Null    

    (* [is_aliased nm] checks if a column name [nm] is aliased. *)
    let is_aliased nm = 
      String.contains nm '.'

    (* [act_name nm] returns the unaliased column name of [nm] *)
    let act_name nm =
      let rn  = ref "" in
      let cnt = ref 0  in  
      (* get alias name *)
      while (!cnt<(String.length nm) && (String.sub nm !cnt 1)<>".") do 
        cnt:=!cnt+1
      done;
      cnt:=!cnt+1; (* skip '.' *)
      (* get actual name *)
      while !cnt<(String.length nm) do 
        rn:=!rn^(String.sub nm !cnt 1);
        cnt:=!cnt+1;
      done; 
      !rn 


    (* checks if the name to match [nm] can be matched by the 
     * column name [cn] *)
    let name_match nm cn = 
      if nm = cn then true  
      (* if the name to check*)
      else if not (is_aliased nm) && (is_aliased cn) then (*if the name appears to be t1.name *)
        if nm = (act_name cn) then true 
        else false  
      else false 

    (* [has_grp_op ast] checks that if a column operation contains 
     * group operation such as sum, max, etc. *)
    let rec has_grp_op ast = 
      match ast with 
      | CName (_,_)    -> false 
      | CDate     _    -> false           
      | CDateTime _    -> false         
      | CTime     _    -> false          
      | CInt      _    -> false          
      | CFloat    _    -> false          
      | CBool     _    -> false            
      | CString   _    -> false             
      | CChar     _    -> false         
      | CNull          -> false        
      | CPlus  (e1,e2) -> has_grp_op e1 || has_grp_op e2 
      | CMinus (e1,e2) -> has_grp_op e1 || has_grp_op e2      
      | CMult  (e1,e2) -> has_grp_op e1 || has_grp_op e2         
      | CDivi  (e1,e2) -> has_grp_op e1 || has_grp_op e2      
      | CGt    (e1,e2) -> has_grp_op e1 || has_grp_op e2          
      | CLt    (e1,e2) -> has_grp_op e1 || has_grp_op e2       
      | CEq    (e1,e2) -> has_grp_op e1 || has_grp_op e2       
      | CNotEq (e1,e2) -> has_grp_op e1 || has_grp_op e2       
      | CGtEq  (e1,e2) -> has_grp_op e1 || has_grp_op e2       
      | CLtEq  (e1,e2) -> has_grp_op e1 || has_grp_op e2       
      | CAnd   (e1,e2) -> has_grp_op e1 || has_grp_op e2      
      | COr    (e1,e2) -> has_grp_op e1 || has_grp_op e2
      | CMod   (e1,e2) -> has_grp_op e1 || has_grp_op e2
      | CNot   e       -> has_grp_op e     
      | CConcat(e1,e2) -> has_grp_op e1 || has_grp_op e2  
      | CSubstr_Index (e1,e2,e3)    -> has_grp_op e1 || has_grp_op e2 || has_grp_op e3
      | CUpper        e             -> has_grp_op e
      | CLower        e             -> has_grp_op e
      | CChar_length  e             -> has_grp_op e
      | CInsert       (e1,e2,e3,e4) -> has_grp_op e1 || has_grp_op e2 || 
                                       has_grp_op e3 || has_grp_op e4
      | CLocate       (e1,e2,e3)    -> (match e3 with
                                       | Some e -> has_grp_op e1 || has_grp_op e2 || has_grp_op e
                                       | None   -> has_grp_op e1 || has_grp_op e2)
      | CTrim         (_,e2,e3)    -> (match e2 with
                                       | Some e -> has_grp_op e || has_grp_op e3
                                       | None   -> has_grp_op e3)
      | CReverse      e             -> has_grp_op e
      | CMax      _    -> true  
      | CMin      _    -> true  
      | CMed      _    -> true  
      | CAvg      _    -> true  
      | CSum      _    -> true 
      | CCount    _    -> true 
      | IsNull    e    -> has_grp_op e 
      | IsNotNull e    -> has_grp_op e

    let rec all_grp_op lst = 
      match lst with
      | []   -> true  
      | (h,_)::t -> if has_grp_op h then all_grp_op t 
                    else false 

    let rec exi_grp_op lst = 
      match lst with
      | []   -> false  
      | (h,_)::t -> if has_grp_op h then true  
                    else exi_grp_op t 

    (* [translate col_op tbl] translates [col_op] into vecAst. *)
    let translate col_op tbl =
      let concat_alias cn = 
        match cn with 
        | CName(a,s) -> begin   
                        match a with 
                        | Some al -> (al^"."^s)
                        | None -> s 
                        end  
        | _ -> "concat_alias cannot be applied here." in  
      let find_idx cn = 
        let i = ref 0 in 
        let ret = ref (-1) in 
        let matched = ref false in 
        while !i < (Array.length tbl.col_attribute) do 
          if name_match cn (fst (tbl.col_attribute.(!i))) then
            if not !matched then 
              ret:=!i
            else( 
              failwith ("Column name "^cn^" is ambigous.")
            )
          else ();
          i:=!i+1
        done; 
        if !ret = (-1) then 
          failwith (tbl.table_name^" does not have column "^cn^".") 
        else 
          !ret in
      let rec trans_help c = 
        match c with 
        | CName (a,s)    -> VCNum (find_idx (concat_alias (CName(a,s)))) 
        | CDate     s    -> VCDate     s          
        | CDateTime n    -> VCDateTime n        
        | CTime     n    -> VCTime     n         
        | CInt      n    -> VCInt      n         
        | CFloat    n    -> VCFloat    n         
        | CBool     n    -> VCBool     n           
        | CString   n    -> VCString   n            
        | CChar     n    -> VCChar     n        
        | CNull          -> VCNull             
        | CPlus  (e1,e2) -> VCPlus  (trans_help e1,trans_help e2) 
        | CMinus (e1,e2) -> VCMinus (trans_help e1,trans_help e2)      
        | CMult  (e1,e2) -> VCMult  (trans_help e1,trans_help e2)         
        | CDivi  (e1,e2) -> VCDivi  (trans_help e1,trans_help e2)      
        | CGt    (e1,e2) -> VCGt    (trans_help e1,trans_help e2)          
        | CLt    (e1,e2) -> VCLt    (trans_help e1,trans_help e2)       
        | CEq    (e1,e2) -> VCEq    (trans_help e1,trans_help e2)       
        | CNotEq (e1,e2) -> VCNotEq (trans_help e1,trans_help e2)       
        | CGtEq  (e1,e2) -> VCGtEq  (trans_help e1,trans_help e2)       
        | CLtEq  (e1,e2) -> VCLtEq  (trans_help e1,trans_help e2)       
        | CAnd   (e1,e2) -> VCAnd   (trans_help e1,trans_help e2)      
        | COr    (e1,e2) -> VCOr    (trans_help e1,trans_help e2)
        | CMod   (e1,e2) -> VCMod   (trans_help e1, trans_help e2)    
        | CConcat(e1,e2) -> VCConcat(trans_help e1, trans_help e2)    
        | CSubstr_Index(e1,e2,e3) -> VCSubstr_Index (trans_help e1,trans_help e2,trans_help e3)
        | CUpper       e          -> VCUpper        (trans_help e)
        | CLower       e          -> VCLower        (trans_help e)
        | CChar_length e          -> VCChar_length  (trans_help e)
        | CInsert   (e1,e2,e3,e4) -> VCInsert       (trans_help e1, trans_help e2, 
                                                     trans_help e3, trans_help e4)
        | CLocate      (e1,e2,e3) -> (match e3 with 
                                      | Some e -> VCLocate (trans_help e1,trans_help e2,Some (trans_help e))
                                      | None   -> VCLocate (trans_help e1,trans_help e2, None))
        | CTrim        (e1,e2,e3) -> (match e2 with
                                      | Some e -> VCTrim (e1, Some (trans_help e), trans_help e3)
                                      | None   -> VCTrim (e1, None        , trans_help e3))
        | CReverse     e          -> VCReverse      (trans_help e)
        | CMax    e      -> VCMax (trans_help e) 
        | CMin    e      -> VCMin (trans_help e) 
        | CMed    e      -> VCMed (trans_help e) 
        | CAvg    e      -> VCAvg (trans_help e) 
        | CSum    e      -> VCSum (trans_help e) 
        | CCount  e      -> VCCount (trans_help e)
        | IsNull    e    -> VCIsNull (trans_help e)
        | IsNotNull e    -> VCIsNotNull (trans_help e)
        | _ -> failwith "[Table]:translate error. Unimplemented col_op."
      in trans_help col_op

  (* [eval_jcond] evalutates the expression in ON clause. *)
  let rec eval_jcond (tb1,al1) (tb2,al2) i1 i2 ast =
    match ast with 
    | IsNull    e -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        if v1 = VNull then VBool true else VBool false  
    | IsNotNull e -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        if v1 <> VNull then VBool true else VBool false  
    | CPlus     (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vadd v1 v2   
    | CMinus    (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vminus v1 v2  
    | CMult     (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vmult v1 v2     
    | CDivi     (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vdivi v1 v2  
    | CGt       (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vgt v1 v2      
    | CLt       (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vlt v1 v2   
    | CEq       (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        veq v1 v2   
    | CNotEq    (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vne v1 v2   
    | CGtEq     (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vge v1 v2   
    | CLtEq     (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vle v1 v2   
    | CAnd      (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vand v1 v2  
    | COr       (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vor v1 v2  
    | CNot      e -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        vnot v1 
    | CMod      (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vmod v1 v2   
    | CConcat   (e1,e2) ->
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in 
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        vconcat v1 v2  
    | CSubstr_Index (e1,e2,e3)    -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        let v3 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e3 in 
        vsubstr_ind v1 v2 v3
    | CUpper        e             -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        vupper v1
    | CLower        e             -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        vlower v1
    | CChar_length  e             -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        vchar_length v1
    | CInsert       (e1,e2,e3,e4) -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        let v3 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e3 in 
        let v4 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e4 in 
        vinsert v1 v2 v3 v4
    | CLocate       (e1,e2,e3)    -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e1 in
        let v2 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e2 in 
        (match e3 with
        | Some e -> vlocate v1 v2 (Some (eval_jcond (tb1,al1) (tb2,al2) i1 i2 e))
        | None   -> vlocate v1 v2 None)
    | CTrim         (e1,e2,e3)    -> 
        let v3 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e3 in 
        (match e2 with
        | Some e -> vtrim e1 (Some (eval_jcond (tb1,al1) (tb2,al2) i1 i2 e)) v3
        | None   -> vtrim e1 None v3)
    | CReverse      e             -> 
        let v1 = eval_jcond (tb1,al1) (tb2,al2) i1 i2 e in 
        vreverse v1
    | CMax  _ | CMin  _ | CMed _ | CAvg _ | CSum _ | CCount _ -> 
        failwith "ON clause cannot have grouping functions. "
    | CInt    n -> VInt    n    
    | CFloat  n -> VFloat  n    
    | CBool   n -> VBool   n    
    | CString n -> VString n    
    | CChar   n -> VChar   n    
    | CNull     -> VNull
    | CName(a,cn) -> 
      (* if column name is unaliased, try to find it in both table. *)
      if a = None then 
        try 
          let _ = translate ast tb1 in 
          let _ = translate ast tb2 in
          raise Not_found 
        with 
        | Failure _ -> 
            let vcn,ttb,idx = 
              try (translate ast tb1,tb1,i1) 
              with _ -> (translate ast tb2,tb2,i2)
            in vcop_eval vcn (ttb.data_vec#at idx)
        | Not_found -> failwith ("Column name "^cn^" is ambigous here.")
      else 
      if a = al1 then 
        let vcn = translate ast tb1 in 
        vcop_eval vcn (tb1.data_vec#at i1)  
      else if a = al2 then 
        let vcn = translate ast tb2 in 
        vcop_eval vcn (tb2.data_vec#at i2)  
      else failwith ("Unsolvable alias "^
                    (match a with 
                     | Some s -> s 
                     | None -> "")^".")

    | _ -> failwith "date/time unimplemented"

  let check_cl cl = 
    let c_ar = Array.of_list cl in 
    let len = Array.length c_ar in 
    for i=0 to len-2 do 
      for j=i+1 to len-1 do 
        if c_ar.(i).col_name = c_ar.(j).col_name then 
          failwith "Duplicate column names."
        else ()
      done
    done

  (*------------------------------------------------------*)              
  (*           End of  Helper Stuff                       *)
  (*------------------------------------------------------*)

  type to_store  = {name:  string; cattri: (string * col_info) array; 
                    idxed: string list; darray: value array array }
  let tbl_to_store tbl = 
    {name   = tbl.table_name;
     cattri = tbl.col_attribute;
     idxed  = tbl.indexed_col;
     darray = tbl.data_vec#to_array}

  (* [exist tn] checks if table with name  [tn] exists in the database. *)
  let exist tn = 
    try 
    let dh = Unix.opendir table_dir in 
    let rec scan () = 
      begin
        try  
          if Unix.readdir dh = (tn^table_postfix) then true
          else scan ()  
        with End_of_file -> false 
      end in 
    scan ()
    with _ -> failwith (table_dir^" does not exists.")

  (* [store_tbl tb] stores a table [tb] into the database directory. *)
  let store_tbl tb = 
    (* Create directory if not exist *)
    let () = 
      try let _ = Unix.opendir db_dir in () with 
      _ -> Unix.mkdir db_dir 0o777 in 
    let () = 
      try let _ = Unix.opendir table_dir in () with
      _ -> Unix.mkdir table_dir 0o777 in 

    let out_ch = open_out (table_dir^(tb.table_name)^table_postfix) in 
    (* flags used for marshaling. *)
    let flags = [Marshal.Closures;Marshal.Compat_32] in
    let sw = tbl_to_store tb in 
    (Marshal.to_channel out_ch sw flags); close_out out_ch

  (* [load_tbl tbn] loads the table with name [tbn] to the memory. *)
  let load_tbl tbn = 
    if exist tbn then( 
      let in_ch = open_in (table_dir^tbn^table_postfix) in 
      let ld = Marshal.from_channel in_ch in 
      let recover (tb:to_store) : t =
        {table_name    = tb.name;
         col_attribute = tb.cattri;
         indexed_col   = tb.idxed;
         data_vec      = Vector.make_array tb.darray}
      in close_in in_ch; recover ld
    )
    else
      failwith ("Table "^tbn^" does not exist.") 


  (* [create_empty n c p] creates an empty table. *)
  let create_empty name cl pri_key = 
    check_cl cl;
    let create_help (ca:col_attri)  = 
    (ca.col_name,
    {dtype = string_to_type ca.data_type;
     not_null  = ca.not_null;
     default   = ca.default;
     auto_incr = ca.auto_incr }) in 
     (* convert a col_attri list to col_info array *)
    let rec info_lst_help lst acc = 
      match lst with 
      | []   -> acc 
      | h::t -> info_lst_help t (Array.append acc [|(create_help h)|]) in 
    (* TODO: Check that auto_increment is only valid for int *)
    {table_name    = name;
     col_attribute = info_lst_help cl [||];
     indexed_col   = if pri_key = "" then [] else [pri_key];
     data_vec      = Vector.make_empty () 
     }

  (* [create_as nm cl tbl] creates a new table out of another
   * table [tbl]. *)
  let create_as nm cl tbl = 
    check_cl cl; 
    let create_help (ca:col_attri)  = 
    (ca.col_name,
    {dtype = string_to_type ca.data_type;
     not_null  = ca.not_null;
     default   = ca.default;
     auto_incr = ca.auto_incr }) in 
     (* convert a col_attri list to col_info array *)
    let rec info_lst_help lst acc = 
      match lst with 
      | []   -> acc 
      | h::t -> info_lst_help t (Array.append acc [|(create_help h)|]) in 
    let find_col cn tbl acc = 
      for i = 0 to (Array.length acc)-1 do 
        for j = 0 to (Array.length tbl.col_attribute)-1 do 
          if (fst cn.(i)) = (fst tbl.col_attribute.(j)) then
            acc.(i) <- j
          else() 
        done
      done in   
    let info_arr = info_lst_help cl [||] in 
    let len = Array.length info_arr in 
    let col_num_map = Array.make len (-1) in 
    find_col info_arr tbl col_num_map;
    let new_vec = tbl.data_vec in 
    for i = 0 to new_vec#tl-1 do 
      let row = Array.make len VNull in 
      for j = 0 to len-1 do
        if col_num_map.(j) <> (-1) then
          row.(j) <- (new_vec#at i).(col_num_map.(j))
        else()
      done 
    done;
    {
      table_name = nm;
      col_attribute = info_arr;
      indexed_col   = [];
      data_vec = new_vec
    }

  (* [drop_table tn] removes the table with name [tn] from the database. *)
  let drop_table tn = 
    let fn = table_dir^tn^table_postfix in 
    if Sys.file_exists fn then 
      Sys.remove fn 
    else 
      failwith (fn^" does not exist.")

  (* [truncate_tbl tbl] truncates all the data in a table.  *)
  let truncate_tbl tbl = 
    tbl.data_vec#truncate


  (* [insert_into tbl lst] insert a row into table 
   * (imperatively adds a row to data vector). [lst] is string * value list. *)
  let insert_into tbl lst = 
    let find_idx cn = 
      let i = ref 0 in 
      let ret = ref (-1) in 
      while !i < (Array.length tbl.col_attribute) do 
        if fst (tbl.col_attribute.(!i)) = cn then
        ret:=!i else ();
        i:=!i+1
      done; !ret in  
    (* new empty row *)
    let new_row = Array.make (Array.length tbl.col_attribute) VNull in 
    (* set default values *) 
    for i = 0 to (Array.length tbl.col_attribute)-1 do 
      new_row.(i) <- (snd tbl.col_attribute.(i)).default
    done; 
    (* set auto_increment value *)
    for i = 0 to (Array.length tbl.col_attribute)-1 do
      if (snd tbl.col_attribute.(i)).auto_incr then 
        new_row.(i) <- (tbl.data_vec#next_val i)
      else () 
    done;
    (* match column name and set values *)
    let rec set_value l = 
      match l with 
      | []   -> ()
      | (n,v)::t -> let idx = find_idx n in 
                      if idx<0 then 
                        failwith ("Table "^
                                  tbl.table_name^
                                  " does not have field "^n^".")
                      else new_row.(idx) <- v; set_value t in 
    let () = set_value lst in 
    (* check not null *)
    for i = 0 to (Array.length tbl.col_attribute)-1 do 
      if (new_row.(i)=VNull && (snd tbl.col_attribute.(i)).not_null ) then 
        failwith ("Field "^(fst tbl.col_attribute.(i))
                 ^" is not allowed to have null value.") 
      else () 
    done; 
    (* check data type *)
    for i = 0 to (Array.length tbl.col_attribute)-1 do 
      if (typematch new_row.(i) (snd tbl.col_attribute.(i)).dtype ) then 
        ()
      else failwith "Insert fails. Type Error."
    done; 
    (* push *)
    tbl.data_vec#push_back new_row

  (* [update tbl cvl wh] updates [tbl] according to the given 
   * column name * value list [cvl] and where condition [wh] *)
  let update tbl cvl wh =
    let find_idx cn = 
      let i = ref 0 in 
      let ret = ref (-1) in 
      while !i < (Array.length tbl.col_attribute) do 
        if fst (tbl.col_attribute.(!i)) = cn then
        ret:=!i else ();
        i:=!i+1
      done; !ret in  
    (* match column name and set values *)
    let rec set_value l new_row = 
      match l with 
      | []   -> ()
      | (n,v)::t -> let idx = find_idx n in 
                      if idx<0 then 
                        failwith ("Table "^
                                  tbl.table_name^
                                  " does not have field "^n^".")
                      else new_row.(idx) <- v; set_value t new_row in 
    (* translate where into a vec_ast *)
    let cop = match wh with 
    | Where (Some a) -> a 
    | Where (None)   -> CBool true in  
    let v_wh = translate cop tbl in 
    let vec = tbl.data_vec in 
    for i = 0 to vec#tl-1 do 
      if vec#eval_where v_wh i then(
        let new_row = vec#at i in 
        set_value cvl new_row;
        vec#set_row new_row i 
      )
      else () 
    done

  (* [delete_from tbl wh] deletes rows in [tbl] that satisfies 
   * the where condition [wh]. *)
  let delete_from tbl wh =   
    (* translate where into a vec_ast *)
    let cop = match wh with 
    | Where (Some a) -> a 
    | Where (None  ) -> CBool true in 
    let v_wh = translate cop tbl in 
    tbl.data_vec#delete v_wh  

  (* [add_col cn str tbl] adds a new nolumn to table [tbl]. *)
  let add_col cn str tbl = 
    let cstr = match cn with ColName s -> s in 
    let typ  = string_to_type str in
    let col_info = 
      {dtype = typ; 
       not_null = false;
       default = VNull;
       auto_incr = false} in 
    let new_col_attr = Array.append tbl.col_attribute [|(cstr,col_info)|] in 
    {table_name    = tbl.table_name;
     col_attribute = new_col_attr;
     indexed_col   = tbl.indexed_col;
     data_vec      = (tbl.data_vec#add_col;tbl.data_vec)}

  (* [drop_col cn tbl] drops the column with name [cn] in table [tbl]  *)
  let drop_col cn tbl = 
    let find_idx cn = 
      let i = ref 0 in 
      let ret = ref (-1) in 
      while !i < (Array.length tbl.col_attribute) do 
        if fst (tbl.col_attribute.(!i)) = cn then
          ret:=!i 
        else ();
        i:=!i+1
      done; !ret in  
    let idx = find_idx cn in 
      if idx<0 then 
        failwith ("Table "^tbl.table_name^" does not have field "^cn^".")
      else(
        let last  = Array.length tbl.col_attribute-1 in 
        let left  = Array.sub tbl.col_attribute 0 idx in 
        let right = Array.sub tbl.col_attribute (idx+1) (last-idx) in 
        {table_name = tbl.table_name;
         col_attribute = Array.append left right;
         indexed_col   = tbl.indexed_col;
         data_vec      = (tbl.data_vec#drop_col idx;tbl.data_vec)
        }
      )


  (* [select] will produce a new table based on all the given conditions.*)
  let select dist cs_lst (tbl,ali) where group having order lim = 
    let is_star c = 
      match c with 
      | CName(_,s) -> if s = "*" then true else false
      | _ -> false in 
    let alias_match cn str = 
      match cn with 
      | CName(a,_) -> begin 
                      match a with 
                      | None     -> true 
                      | Some ali -> let r = Str.regexp (ali^"\\..+") in 
                                    Str.string_match r str 0 
                      end 
      | _ -> failwith "alias_match cannot be applied here." in 
    let is_dist = match dist with Distinct b -> b in 
    let rec vast_help vl acc = 
      match vl with 
      | []   -> acc 
      | (h,_)::t -> if is_star h then( 
                      let all_col = ref [] in
                      for i = 0 to (Array.length tbl.col_attribute) -1 do 
                        (* check alias *)
                        if alias_match h (fst tbl.col_attribute.(i)) then 
                          all_col:= !all_col@[VCNum i]
                        else () 
                      done; 
                      vast_help t (acc@(!all_col)) )
                    else vast_help t (acc@[translate h tbl]) in 
    let rec ord_help vl acc = 
      match vl with 
      | []   -> acc 
      | h::t -> if is_star h then( 
                      let all_col = ref [] in
                      for i = 0 to (Array.length tbl.col_attribute) -1 do 
                        (* check alias *)
                        if alias_match h (fst tbl.col_attribute.(i)) then 
                          all_col:= !all_col@[VCNum i]
                        else () 
                      done; 
                      ord_help t (acc@(!all_col)) )
                    else ord_help t (acc@[translate h tbl]) in
    let elim_dup_cn tbl = 
      let len = Array.length tbl.col_attribute in 
      for i = 0 to len-2 do 
        let cnt = ref 1 in 
        for j = i+1 to len-1 do 
          if (fst tbl.col_attribute.(i)) = (fst tbl.col_attribute.(j)) then(
            let new_name = ((fst tbl.col_attribute.(j))^(string_of_int !cnt)) in
            cnt := !cnt+1; 
            tbl.col_attribute.(j) <- (new_name, snd  tbl.col_attribute.(j))
          )
          else()
        done
      done in 
    let vast_lst = vast_help cs_lst [] in 
    let wh_cond = 
      match where with 
      | Where (Some c) -> translate c tbl 
      | Where None     -> VCBool true in 
    let grp_cond,grp_ord = 
      match group with GroupBy (gl,gord) -> (ord_help gl [],gord) in 
    let hav_cond = 
      match having with 
      | Having (Some c) -> if grp_cond = [] then 
                            failwith "GROUP BY clause missing."
                           else translate c tbl
      | Having None     -> VCBool true in 

    (* sample the new column names *)
    let rec sample_names cl acc =
      match cl with 
      | [] -> Array.of_list acc 
      | (h,s)::t -> if is_star h then( (*TODO:check if star has sub name. *)
                      let all_cn = ref [] in 
                      for i = 0 to (Array.length tbl.col_attribute)-1 do 
                        if alias_match h (fst tbl.col_attribute.(i)) then 
                          all_cn:= !all_cn@[fst tbl.col_attribute.(i)]
                        else () 
                      done; 
                      sample_names t (acc@(!all_cn)) )
                    else 
                      match s with 
                      | Some sb  -> sample_names t (acc@[sb])
                      | None     -> begin
                                    match h with 
                                    | CName(_,cn) -> sample_names t (acc@[cn])
                                    | _       -> sample_names t (acc@["field"])
                                    end in 
    let new_names = sample_names cs_lst [] in 
    (* get the resultant vector *)
    let new_vec = tbl.data_vec in 
    (* order by *) 
    let ord_lst, ord_typ = match order with OrderBy (l,t) -> (l,t) in 
    if ord_lst <> [] then(
      let vo_lst = ord_help ord_lst [] in 
      new_vec#sort vo_lst ord_typ
    ) 
    else ();

    let new_vec = 
      (* If there is no group by condition, normal select. *) (* TODO: CHECK SUM/COUNT/..... *)
      if grp_cond = [] then(
        if exi_grp_op cs_lst then(
          if all_grp_op cs_lst then 
            new_vec#grp_select is_dist vast_lst ([VCBool true]) hav_cond lim
          else 
            failwith "GROUP BY clause expected."
        )
        else(
          new_vec#vec_select is_dist vast_lst wh_cond lim
        )
      )
      (* else use grp_sel *)
      else( 
          new_vec#sort grp_cond grp_ord;
          new_vec#grp_select is_dist vast_lst grp_cond hav_cond lim
      ) 
    in
    (* sample column data types *)
    let sample_type r = 
      let cd = ref [] in 
      for i = 0 to (Array.length r)-1 do 
        cd:= !cd@[typeof r.(i)]
      done; 
      Array.of_list !cd in 
    let new_types = 
      if new_vec#tl = 0 then( Array.make (Array.length new_names) `Null )
      else sample_type (new_vec#at 0) in 
    (* construct new column attribute *)
    let new_col_attr = ref [] in 
    for i = 0 to (Array.length new_names)-1 do 
      let cinfo = {dtype=new_types.(i); not_null =false; 
                         default=VNull; auto_incr=false} in 
      new_col_attr := !new_col_attr@[(new_names.(i),cinfo)]
    done;
    let new_tbl = {table_name   = "";
                   col_attribute= Array.of_list !new_col_attr;
                   indexed_col  = [];
                   data_vec     = new_vec} in

    (* if the table is aliased, remove the alias(es) *)
    let rmv_ali ca lst = 
      let ret = Array.copy ca in
      for i = 0 to (Array.length ret)-1 do 
        let nm = fst ret.(i) in 
        if String.contains nm '.' then (*if the name appears to be t1.name *)
          let a  = ref "" in 
          let rn = ref "" in
          let cnt = ref 0 in  
          (* get alias name *)
          while (!cnt<(String.length nm) && (String.sub nm !cnt 1)<>".") do 
            a:=!a^(String.sub nm !cnt 1);
            cnt:=!cnt+1;
          done;
          cnt:=!cnt+1; (* skip '.' *)
          (* get actual name *)
          while !cnt<(String.length nm) do 
            rn:=!rn^(String.sub nm !cnt 1);
            cnt:=!cnt+1;
          done;
          if List.exists (fun x -> x = Some !a) lst then 
            ret.(i) <- (!rn,snd ret.(i))
          else failwith "this is possible??" 
        else () 
      done; ret in 
      let to_ret =  
        {table_name   = "derived table";
         col_attribute= rmv_ali new_tbl.col_attribute ali;
         indexed_col  = [];
         data_vec     = new_tbl.data_vec} in 
      elim_dup_cn to_ret; to_ret 

  
  (* [from fr] loads a table based on the from_obj [fr]. *)
  let rec from fr = 
    let exp, sn, jn = match fr with From(e,s,j) -> (e,s,j) in 
    let add_alias tb sn = 
      let col_attr = Array.copy tb.col_attribute in
      let alias_name = match sn with Some s -> s | None -> "" in 
      for i=0 to (Array.length col_attr)-1 do 
        let cn,ci = col_attr.(i) in 
        if alias_name <> "" then 
          col_attr.(i) <- (alias_name^"."^cn,ci)
        else ()
      done;
      {table_name=tb.table_name;
       col_attribute = col_attr;
       indexed_col = tb.indexed_col;
       data_vec = tb.data_vec} in 
    let ori_tbl = 
      match exp with 
      | TName s  -> load_tbl s 
      | Select(d,csl,fr,wh,gr,hv,od,lm) -> 
          (*TODO: every derived table must have alias.*)
          let tmp,al = from fr in 
            select d csl (tmp,al) wh gr hv od lm
      | _ -> failwith "Syntax error." in 
      match jn with 
      | NoJoin -> (add_alias ori_tbl sn,[sn])
      | LJoin(e1,al,jc) -> 
        begin
          let tmp,_ = from (From (e1,al,NoJoin)) in 
          let ret = letf_join (add_alias ori_tbl sn,sn) (tmp,al) jc
          in (ret,[sn;al])
        end
      | RJoin(e1,al,jc) -> 
        begin
          let tmp,_ = from (From (e1,al,NoJoin)) in 
          let ret = right_join (add_alias ori_tbl sn,sn) (tmp,al) jc
          in (ret,[sn;al])
        end
      | IJoin(e1,al,jc) -> 
        begin
          let tmp,_ = from (From (e1,al,NoJoin)) in 
          let ret = inner_join (add_alias ori_tbl sn,sn) (tmp,al) jc
          in (ret,[sn;al])
        end

  and letf_join (tb1,al1) (tb2,al2) jcond =  
    if al1 = None || al2 = None then 
      failwith "JOIN objects must have an alias."
    else
      let jexp = 
        match jcond with 
        | On (Some e) -> e 
        | On (None  ) -> CBool true in 
      let left_help i j = 
        let v = eval_jcond (tb1,al1) (tb2,al2) i j jexp in 
        match v with 
        | VBool b -> b 
        | _ -> failwith "ON clause should have type bool." in 
      (* compute the new data vector *)
      let new_vec = Vector.make_empty () in 
      let vec1 = tb1.data_vec in 
      let vec2 = tb2.data_vec in 
      let wid  = Array.length tb2.col_attribute in 
      for i = 0 to tb1.data_vec#tl-1 do 
        let fnd = ref false in 
        for j = 0 to tb2.data_vec#tl-1 do 
          if left_help i j then(
            new_vec#push_back (Array.append (vec1#at i) (vec2#at j));
            fnd := true 
          )
          else ()
        done;
        if not !fnd then 
          new_vec#push_back (Array.append (vec1#at i) (Array.make wid VNull))
        else ()
      done;
      {table_name = "joint table";
       col_attribute = Array.append (tb1.col_attribute) (tb2.col_attribute);
       indexed_col = [];
       data_vec = new_vec
       }

  and right_join (tb1,al1) (tb2,al2) jcond =  
    if al1 = None || al2 = None then 
      failwith "JOIN objects must have an alias."
    else
      let jexp = 
        match jcond with 
        | On (Some e) -> e 
        | On (None  ) -> CBool true in 
      let right_help i j = 
        let v = eval_jcond (tb1,al1) (tb2,al2) i j jexp in 
        match v with 
        | VBool b -> b 
        | _ -> failwith "ON clause should have type bool." in 
      (* compute the new data vector *)
      let new_vec = Vector.make_empty () in 
      let vec1 = tb1.data_vec in 
      let vec2 = tb2.data_vec in 
      let wid  = Array.length tb1.col_attribute in 
      for i = 0 to tb2.data_vec#tl-1 do 
        let fnd = ref false in 
        for j = 0 to tb1.data_vec#tl-1 do 
          if right_help i j then(
            new_vec#push_back (Array.append (vec1#at i) (vec2#at j));
            fnd := true 
          )
          else ()
        done;
        if not !fnd then 
          new_vec#push_back (Array.append (Array.make wid VNull) (vec2#at i) )
        else ()
      done;
      {table_name = "joint table";
       col_attribute = Array.append (tb1.col_attribute) (tb2.col_attribute);
       indexed_col = [];
       data_vec = new_vec
       }

  and inner_join (tb1,al1) (tb2,al2) jcond =  
    if al1 = None || al2 = None then 
      failwith "JOIN objects must have an alias."
    else
      let jexp = 
        match jcond with 
        | On (Some e) -> e 
        | On (None  ) -> CBool true in 
      let left_help i j = 
        let v = eval_jcond (tb1,al1) (tb2,al2) i j jexp in 
        match v with 
        | VBool b -> b 
        | _ -> failwith "ON clause should have type bool." in 
      (* compute the new data vector *)
      let new_vec = Vector.make_empty () in 
      let vec1 = tb1.data_vec in 
      let vec2 = tb2.data_vec in 
      for i = 0 to tb1.data_vec#tl-1 do 
        for j = 0 to tb2.data_vec#tl-1 do 
          if left_help i j then(
            new_vec#push_back (Array.append (vec1#at i) (vec2#at j))
          )
          else ()
        done
      done;
      {table_name = "joint table";
       col_attribute = Array.append (tb1.col_attribute) (tb2.col_attribute);
       indexed_col = [];
       data_vec = new_vec
       }


  let raw_data tbl = 
    tbl.data_vec#to_array

  let get_vec tbl = tbl.data_vec

  class table_printer = 
  object(s)
    val mutable max_lengths   = [| |]
    val mutable print_buffer  = [| |]
    val mutable num_of_col    = 0 
    val mutable buffer_size   = 0 

    (* [get_str v] convert a value to string. *)
    method private get_str v = 
      match v with 
      | VInt     n -> string_of_int n   
      | VFloat   f -> string_of_float f 
      | VBool    b -> string_of_bool b   
      | VString  s -> s 
      | VChar    c -> String.make 1 c  
      | VNull      -> " NULL"
      | _          -> failwith "Unimplemented data type."    
      (* TODO: 
      | VDate     of string
      | VDateTime of string
      | VTime     of string *)

    (* [get_col_names ca] initialize the print buffer. *)
    method private get_col_names (col_attri:(string * col_info) array) =
      num_of_col   <- Array.length col_attri;
      max_lengths  <- Array.make num_of_col 0;
      print_buffer <- [| [| |] |];
      buffer_size  <- 0; 
      let tmp = Array.make num_of_col "" in 
      for i = 0 to num_of_col-1 do 
        let cname = fst col_attri.(i) in 
        let len = String.length cname in 
        max_lengths.(i) <- len;
        tmp.(i) <- cname 
      done; 
      print_buffer.(0) <- tmp;
      buffer_size <- buffer_size + 1

    method private read_row va = 
      let tmp = Array.make num_of_col "" in 
      for i = 0 to num_of_col-1 do
        let str = s#get_str va.(i) in 
        let len = String.length str in 
          (* update max_lengths*)
          if max_lengths.(i)<len then 
            max_lengths.(i) <- len
          else ();  
        tmp.(i) <- str 
      done;
      print_buffer <- Array.append print_buffer [| tmp |];
      buffer_size <- buffer_size + 1

    (* [print_hline] prints out a line like +---+------+-----+*)
    method private print_hline = 
      print_string "+";
      for i = 0 to num_of_col-1 do 
        for _ = 0 to max_lengths.(i) do 
          print_string "-"
        done;
        print_string "+"
      done;
      print_string "\n"

    (* [print_buffer] prints out a row in the buffer. *)
    method private print_buffer = 
      (* print out column names. *)
      s#print_hline;
      print_string "|";
      for j = 0 to num_of_col-1 do 
        let to_print = print_buffer.(0).(j) in 
        let len = String.length to_print in 
        print_string to_print;
        for _ = 0 to max_lengths.(j)-len do 
          print_string " "
        done;
        print_string "|" 
      done;
      print_string "\n";
      s#print_hline;
      (* print out data *)
      for i = 1 to buffer_size-1 do 
        print_string "|";
        for j = 0 to num_of_col-1 do 
          let to_print = print_buffer.(i).(j) in 
          let len = String.length to_print in 
          print_string to_print;
          for _ = 0 to max_lengths.(j)-len do 
            print_string " "
          done;
          print_string "|" 
        done;
        print_string "\n" 
      done;
      s#print_hline

    (* [read_tbl tbl] will puts the column name and data into the buffer. *)
    method private read_tbl tbl = 
      let vec = tbl.data_vec in 
      s#get_col_names tbl.col_attribute;
      for i = 0 to vec#tl-1 do 
        s#read_row (vec#at i)
      done;

    method print tbl = 
      s#read_tbl tbl;
      s#print_buffer

  end (* end of class buffer *)

  let print_table (tbl:t) = 
    let buf = new table_printer in 
    buf#print tbl 

  let test_print () = ()
  (* place testing print functions here *)

end 
(* end of Table module *)
