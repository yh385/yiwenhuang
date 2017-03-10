open Ast 
open VecAst

class type vector =
  object 
    val mutable data : (value array) array
    val mutable tail : int 
    val mutable last : int 

    method tl   : int  
    method size : int  
    (* method insert    : (value array) -> int -> unit  *)         
    method push_back : value array-> unit                             
    method set_row   : value array -> int -> unit                      
    method delete    : vec_col_op -> unit                             
    method add_col   : unit                                             
    method drop_col  : int -> unit                                    
    method eval_where: vec_col_op -> int -> bool                        
    method at        : int -> value array                                  
    method next_val  : int -> value                                           
    method eliminate_dup : unit                                              
    method truncate    : unit                                               
    method vec_select  : bool -> vec_col_op list -> vec_col_op -> 
                         limit_num -> vector 
    method sort        : vec_col_op list -> [`Asc | `Desc] -> unit 
    method grp_select  : bool -> vec_col_op list 
                              -> vec_col_op list -> vec_col_op -> 
                         limit_num -> vector
    method to_array  : value array array

  end

module type RowVector = sig 

  val make_empty  : unit -> vector 
  val make_array  : value array array -> vector 

end


module Vector : RowVector = struct
  (* ----------------------------------------------------------- *)
  (*                      Helper Stuff                           *)
  (* ----------------------------------------------------------- *)

  (* This constant specifies by how much we will increase the size 
   * of our vector once the vector is full.  *)
  let inc_size = 256
  (* [vec_init] is a helper type 
   * enabling us to have different ways to initialize a vector. *)
  type vec_init = | Empty | VArray of value array array

  let cmp_lt v1 v2 = 
    match v1,v2 with 
    | VNull,_               -> false
    | _,VNull               -> false
    | VInt    x, VInt    y  -> (x < y)
    | VInt    x, VFloat  y  -> ((float_of_int x) < y)
    | VFloat  x, VInt    y  -> (x < (float_of_int y))
    | VFloat  x, VFloat  y  -> (x < y)
    | VString x, VString y  -> (x < y)
    | VChar   x, VChar   y  -> (x < y)
    | VString x, VChar   y  -> (x < (String.make 1 y))
    | VChar   x, VString y  -> ((String.make 1 x) < y)
    | VBool b1, VBool b2    -> (b1<b2)
    | _,_ -> failwith "[Vector]: type error in cmp_lt."

  let cmp_gt v1 v2 = 
    match v1,v2 with 
    | VNull,_               -> false
    | _,VNull               -> false
    | VInt    x, VInt    y  -> (x > y)
    | VInt    x, VFloat  y  -> ((float_of_int x) > y)
    | VFloat  x, VInt    y  -> (x > (float_of_int y))
    | VFloat  x, VFloat  y  -> (x > y)
    | VString x, VString y  -> (x > y)
    | VChar  x, VChar  y    -> (x > y)
    | VString x, VChar  y   -> (x > (String.make 1 y))
    | VChar  x, VString y   -> ((String.make 1 x) > y)
    | VBool b1, VBool b2    -> (b1>b2)
    (* TODO: implement Date/Time/DateTime*)
    | _,_ -> failwith "[Vector]: type error in cmp_gt."    

  let cmp_eq v1 v2 = 
    match v1,v2 with 
    | VNull,_               -> false
    | _,VNull               -> false
    | VInt    x, VInt    y  -> (x = y)
    | VInt    x, VFloat  y  -> ((float_of_int x) = y)
    | VFloat  x, VInt    y  -> (x = (float_of_int y))
    | VFloat  x, VFloat  y  -> (x = y)
    | VString x, VString y  -> (x = y)
    | VChar  x, VChar  y    -> (x = y)
    | VString x, VChar  y   -> (x = (String.make 1 y))
    | VChar  x, VString y   -> ((String.make 1 x) = y)
    | VBool b1, VBool b2    -> (b1=b2)
    (* TODO: implement Date/Time/DateTime*)
    | _,_ -> failwith "[Vector]: type error in cmp_eq."    

  let row_eq r1 r2 = 
    let ret = ref true in 
    let brk = ref false in
    let cnt = ref 0 in  
    let n   = Array.length r1 in 
    while (not !brk) && (!cnt < n) do 
      if cmp_eq r1.(!cnt) r2.(!cnt) then(
        cnt:= !cnt+1
      )
      else(
        ret:=false;
        brk:=true
      )
    done; !ret 
   
  (* ----------------------------------------------------------- *)
  (*               End of Helper Stuff                           *)
  (* ----------------------------------------------------------- *)

  class vec (init:vec_init) : vector = 
    (* initializer/constructor *)
    let ini_data = ref [| [||] |] in 
    let ini_tail = ref 0 in 
    let ini_is_empty = ref true in 
    let _ = 
    match init with 
    | Empty -> 
      ini_tail  := 0;
    | VArray a ->( 
      ini_is_empty := false;           (* set is_empty to be false       *)
      ini_data:= Array.copy a;         (* perform deep copy              *)
      ini_data:= Array.append !ini_data [| [| |] |]; (* add a dummy node *)
      try                              (* initialize tail pointer *)
      for i = 0 to ((Array.length !ini_data)-1) do 
        if (!ini_data).(i) = [| |] then 
          (ini_tail:=i; 
          failwith "")
        else () 
      done 
      with _ -> () ) 
    in
  object(s)

    (* [data] is the array that holds all data. *)
    val mutable data = !ini_data
    (* [tail] is a pointer always pointing to the 
     * last valid element of the vector. *) 
    val mutable tail = !ini_tail
    (* [size] keeps a record of the actuall size of the vector. *)
    val mutable last = (Array.length !ini_data)-1

    val mutable is_empty  = !ini_is_empty
    (* val mutable col_types = !ini_col_type *)

    (*                    *)
    (*  Private Methods   *)
    (*                    *)

    (* [rmv_row i] removes the i-th (0-based) column.*)
    method private rmv_row i = 
      let front = Array.sub data 0 i in 
      let back  = Array.sub data (i+1) (tail-i-1) in 
      data <- Array.append front back;
      tail <- tail - 1

    method private swap i j =
      let tmp = Array.copy data.(i) in 
      data.(i) <- data.(j);
      data.(j) <- tmp 

    method private mark_grp lst = 
      let rec grp_help l idx acc i= 
        match l with
        | []   -> acc 
        | h::t -> acc.(i) <- (grp_eval h data (idx,idx)); 
                              grp_help t idx acc (i+1) in
      let ret = ref [] in 
      let lf  = ref 0 in 
      let rt  = ref 0 in 
      let len = List.length lst in 
      while !rt<tail do 
        let vlf = grp_help lst !lf 
                  (Array.make len VNull) 0 in 
        let vrt = grp_help lst !rt 
                  (Array.make len VNull) 0 in
        if !lf = !rt then(
          rt:=!rt+1
        )
        else( 
          if row_eq vlf vrt then(
            rt:=!rt+1
          )
          else(
            ret:= !ret@[(!lf,!rt-1)];
             lf:= !rt  
          )
        )
      done;
      ret:= !ret@[(!lf,!rt-1)];
      !ret 

    (* [find_max i] returns the maximum value in i-th column. *)
    method private find_max cnum =
      if data.(0) = [| |] then (VInt 0) 
      else(
        let m = ref data.(0).(cnum) in 
        for i = 0 to tail-1 do 
          if (vgt data.(i).(cnum) !m) = (VBool true) then 
            m:=data.(i).(cnum)
          else ()
        done;
        !m 
      )
    (*      End  of       *)
    (*  Private Methods   *)
    (*                    *)


    (* These two methods are used for debugging. Should be removed once 
     * once the implementation is done. *)
    method tl   = tail 
    method size = last 

    method push_back row = 
      (* Step 1. Check if the vector is empty. 
       * If so, then initialize col_types for further type checking. 
       * If not, check if the input row is valid. Raise Failure if invalid. *)
      let _ = 
        if is_empty then 
          let _ = 
          (* col_types <- type_array_of [| row |]; *)
          data <- Array.append data (Array.make inc_size [| |]);
          last <- last + inc_size;
          is_empty <- false in ()
        else
          () in 
      (* Step 2. Now that we can make sure the input row is valid, 
       * we can add it to our [data]. *)
        data.(tail) <- row;
        tail <- tail + 1;
        (* If the vector is full after insertion: *)
        if tail > last then 
          let _ = 
          data <- Array.append data (Array.make inc_size [| |]);
          last <- last + inc_size in () 
        else ()

    method eval_where wh i = 
      match vcop_eval wh data.(i) with 
      | VBool b ->  b 
      | _ -> failwith "WHERE clause should have type bool."

    (* [set_row new_row] will set data.(i) to be new_row. 
     * Used in update statement. *)
    method set_row new_row i = 
      data.(i) <- new_row

    (* [delete wh] will remove all the rows satisfying 
     * the where conditon [wh]*)
    method delete wh = 
      let cnt = ref 0 in 
      while !cnt < tail do 
        if s#eval_where wh !cnt then(
          s#rmv_row !cnt
        )
        else(
          cnt := !cnt + 1
        )
      done 

    (* [add_col] will append another column to the table. *)
    method add_col = 
      for i = 0 to tail -1 do 
        data.(i) <- Array.append data.(i) [| VNull |]
      done 

    (* [drop_col i] will remove the i-th column form the table. *)
    method drop_col idx = 
      let wid = Array.length data.(0) in 
      for i = 0 to tail-1 do 
        let left = Array.sub data.(i) 0 idx in 
        let right = Array.sub data.(i) (idx+1) (wid-idx-1) in 
        data.(i) <- Array.append left right 
      done 


    (* [at i] returns the i-th row. Raise failure if index is invalid. *)
    method at i = 
      if i < 0 || i >= tail then failwith "[Vector]: Invalid index."
      else Array.copy data.(i)

    (* [next_val i] returns next value in i-th column. 
     * Used for auto increment. Note that the i-th column can only be int. *)
    method next_val i = 
      let v = s#find_max i in 
      vadd v (VInt 1)


    (* [eliminate_dup] removes all the duplicated rows in the vector. *)
    method eliminate_dup = 
      let new_data_lst = ref [] in 
      for i = 0 to tail-1 do 
        let has_dup = ref false in 
          for j=i+1 to tail-1 do 
            if data.(i) = data.(j) then( 
            has_dup:= true)
            else ()
          done;
        if not !has_dup then( 
          new_data_lst:=((!new_data_lst)@[data.(i)]))
        else ()
      done;
      let new_data_ar = Array.of_list !new_data_lst in 
      data <- Array.append new_data_ar [| [| |] |];
      tail <- Array.length new_data_ar;
      last <- (Array.length data) - 1 

    (* [truncate] truncates all the data in the vector. *)
    method truncate = 
      data <- [| [||] |];
      tail <- 0;
      last <- 0 

    (* vector select without grouping. If there is no where condition,
     * where should be VCBool true *)
    method vec_select dist lst where lim_num = 
      let sel_num = 
        match lim_num with 
        | Limit n -> (min (n-1) (tail-1)) 
        | NoLimit -> tail-1 in 
      (* [sel_help] evaluates a list of vec_col_op and produce a new 
       * row. *)
      let rec sel_help l r acc i= 
        match l with
        | []   -> acc 
        | h::t -> acc.(i) <- (vcop_eval h r); sel_help t r acc (i+1) in 
      let new_length = List.length lst in 
      let ret = new vec Empty in 
      for i = 0 to sel_num do 
        if s#eval_where where i then( 
          ret#push_back (sel_help lst data.(i) (Array.make new_length VNull) 0))
        else ()
      done; 
      if dist then
        ret#eliminate_dup 
      else ();
      ret

    method sort lst ord = 
      let row_len = Array.length data.(0) in 
      if row_len = 0 then () else 
      let rec eval_lst l r acc i = 
        match l with
        | []   -> acc 
        | h::t -> acc.(i) <- (vcop_eval h r); eval_lst t r acc (i+1) in 
      let get_fun order = 
        if order = `Asc then 
          (fun r1 r2 -> 
            let nr1 = eval_lst lst r1 (Array.make row_len VNull) 0 in 
            let nr2 = eval_lst lst r2 (Array.make row_len VNull) 0 in 
            let ret = ref true in 
            let brk = ref false in 
            let cnt = ref 0 in 
            while (not !brk) && !cnt < (Array.length nr1)-1 do 
              if cmp_lt nr1.(!cnt) nr2.(!cnt) then(
                ret:=true;
                brk:=true
              )
              else if cmp_eq nr1.(!cnt) nr2.(!cnt) then()              
              else (
                ret:=false;
                brk:=true              
              );
              cnt:= !cnt + 1
            done; !ret)
        else 
          (fun r1 r2 -> 
            let nr1 = eval_lst lst r1 (Array.make row_len VNull) 0 in 
            let nr2 = eval_lst lst r2 (Array.make row_len VNull) 0 in 
            let ret = ref true in 
            let brk = ref false in 
            let cnt = ref 0 in 
            while (not !brk) && !cnt < (Array.length nr1)-1 do 
              if cmp_gt nr1.(!cnt) nr2.(!cnt) then( 
                ret:=true;
                brk:=true
              )
              else if cmp_eq nr1.(!cnt) nr2.(!cnt) then()  
              else (
                ret:=false;
                brk:=true
              );
              cnt:= !cnt + 1
            done; !ret)
          in 
      let row_cmp = get_fun ord in 
      (* Using Quicksort *)
      let partition start _end = 
        let piviot = data.(_end) in 
        let piv_idx = ref start in 
        for i = start to _end-1 do
          if row_cmp data.(i) piviot then(
            s#swap i !piv_idx;
            piv_idx:= !piv_idx + 1)
          else () 
        done; 
        s#swap !piv_idx _end;
        !piv_idx in         
      let rec quick_sort start _end = 
        if start >= _end then () 
        else(
          let pidx = partition start _end in 
          quick_sort start (pidx-1);
          quick_sort (pidx+1) _end   ) in 
      quick_sort 0 (tail-1)

    (* [grp_select] is used when there is Group by clause. *)
    method grp_select dist sel_lst grp_lst having lim_num = 
      let interval = Array.of_list (s#mark_grp grp_lst) in 
      let sel_num = 
        match lim_num with 
        | Limit n -> (min (n-1) ((Array.length interval)-1)) 
        | NoLimit -> (Array.length interval)-1 in 
      let extract_bool v = 
        match v with 
        | VBool b -> b 
        | _       -> failwith "HAVING clause should have type bool."  in 
      let rec gs_help l (lf,rt) acc i= 
        match l with
        | []   -> acc 
        | h::t -> acc.(i) <- (grp_eval h data (lf,rt)); gs_help t (lf,rt) acc (i+1) in 
      let len = List.length sel_lst in 
      let ret = new vec Empty in
      for i = 0 to sel_num do 
        let l,r = interval.(i) in 
        if extract_bool ( grp_eval having data (l,r) ) then 
          ret#push_back ( gs_help sel_lst (l,r) (Array.make len VNull) 0)
        else ()        
      done;
      if dist then
        ret#eliminate_dup 
      else ();
      ret 

    method to_array = 
      (* Return a deep copied array, just to be safe *)
      Array.sub data 0 tail  
  end 
  (***** end of class vec *****)


  let make_empty () = new vec Empty
  let make_array a  = new vec (VArray a)   

end (* end of module Vector *)
