open Ast 
type vec_col_op = 
  | VCIsNull    of vec_col_op
  | VCIsNotNull of vec_col_op
  | VCPlus     of vec_col_op * vec_col_op 
  | VCMinus    of vec_col_op * vec_col_op
  | VCMult     of vec_col_op * vec_col_op   
  | VCDivi     of vec_col_op * vec_col_op
  | VCGt       of vec_col_op * vec_col_op    
  | VCLt       of vec_col_op * vec_col_op 
  | VCEq       of vec_col_op * vec_col_op 
  | VCNotEq    of vec_col_op * vec_col_op 
  | VCGtEq     of vec_col_op * vec_col_op 
  | VCLtEq     of vec_col_op * vec_col_op 
  | VCAnd      of vec_col_op * vec_col_op
  | VCOr       of vec_col_op * vec_col_op
  | VCNot      of vec_col_op  
  | VCMod      of vec_col_op * vec_col_op 
  | VCConcat       of vec_col_op * vec_col_op 
  | VCSubstr_Index of vec_col_op * vec_col_op * vec_col_op
  | VCUpper        of vec_col_op
  | VCLower        of vec_col_op
  | VCChar_length  of vec_col_op
  | VCInsert       of vec_col_op * vec_col_op * vec_col_op * vec_col_op
  | VCLocate       of vec_col_op * vec_col_op * vec_col_op option
  | VCTrim         of trim_obj * vec_col_op option * vec_col_op
  | VCReverse      of vec_col_op
  | VCMax      of vec_col_op  
  | VCMin      of vec_col_op  
  | VCMed      of vec_col_op 
  | VCAvg      of vec_col_op 
  | VCSum      of vec_col_op 
  | VCCount    of vec_col_op
  | VCNum      of int
  | VCDate     of string  
  | VCDateTime of string  
  | VCTime     of string  
  | VCInt      of int     
  | VCFloat    of float   
  | VCBool     of bool    
  | VCString   of string  
  | VCChar     of char    
  | VCNull   

let vadd v1 v2 = 
  match v1,v2 with 
  | VInt   x, VInt   y -> VInt   (x +  y)
  | VInt   x, VFloat y -> VFloat ((float_of_int x) +. y)
  | VFloat x, VInt   y -> VFloat (x +. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x +. y)
  | _ -> failwith "add ERROR"  

let vminus v1 v2 = 
  match v1,v2 with 
  | VInt   x, VInt   y -> VInt   (x -  y)
  | VInt   x, VFloat y -> VFloat ((float_of_int x) -. y)
  | VFloat x, VInt   y -> VFloat (x -. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x -. y)
  | _ -> failwith "minus ERROR"

let vmult v1 v2 = 
  match v1,v2 with 
  | VInt   x, VInt   y -> VInt   (x *  y)
  | VInt   x, VFloat y -> VFloat ((float_of_int x) *. y)
  | VFloat x, VInt   y -> VFloat (x *. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x *. y)
  | _ -> failwith "VCMult ERROR"

let vdivi v1 v2 = 
  match v1,v2 with 
  | VInt   x, VInt   y -> VFloat ((float_of_int x) /. (float_of_int y))
  | VInt   x, VFloat y -> VFloat ((float_of_int x) /. y)
  | VFloat x, VInt   y -> VFloat (x /. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x /. y)
  | _ -> failwith "VCDivi ERROR"

let vmod v1 v2 = 
  match v1,v2 with 
  | VInt x, VInt y -> VInt (x mod y)
  | _,_ -> failwith "mod can only be applied on integers."  

let vlt v1 v2 = 
  match v1,v2 with 
  | VNull,_               -> VBool false
  | _,VNull               -> VBool false
  | VInt    x, VInt    y  -> VBool (x < y)
  | VInt    x, VFloat  y  -> VBool ((float_of_int x) < y)
  | VFloat  x, VInt    y  -> VBool (x < (float_of_int y))
  | VFloat  x, VFloat  y  -> VBool (x < y)
  | VString x, VString y  -> VBool (x < y)
  | VChar   x, VChar   y  -> VBool (x < y)
  | VString x, VChar   y  -> VBool (x < (String.make 1 y))
  | VChar   x, VString y  -> VBool ((String.make 1 x) < y)
  | VBool b1, VBool b2    -> VBool (b1<b2)
  | _,_ -> failwith "[Vector]: type error in cmp_lt."

 let vgt v1 v2 = 
   match v1,v2 with 
   | VNull,_               -> VBool false
   | _,VNull               -> VBool false
   | VInt    x, VInt    y  -> VBool (x > y)
   | VInt    x, VFloat  y  -> VBool ((float_of_int x) > y)
   | VFloat  x, VInt    y  -> VBool (x > (float_of_int y))
   | VFloat  x, VFloat  y  -> VBool (x > y)
   | VString x, VString y  -> VBool (x > y)
   | VChar  x, VChar  y    -> VBool (x > y)
   | VString x, VChar  y   -> VBool (x > (String.make 1 y))
   | VChar  x, VString y   -> VBool ((String.make 1 x) > y)
   | VBool b1, VBool b2    -> VBool (b1>b2)
   (* TODO: implement Date/Time/DateTime*)
   | _,_ -> failwith "[Vector]: type error in cmp_gt."  

let veq v1 v2 = 
  match v1,v2 with 
  | VNull,_               -> VBool false
  | _,VNull               -> VBool false
  | VInt    x, VInt    y  -> VBool (x = y)
  | VInt    x, VFloat  y  -> VBool ((float_of_int x) = y)
  | VFloat  x, VInt    y  -> VBool (x = (float_of_int y))
  | VFloat  x, VFloat  y  -> VBool (x = y)
  | VString x, VString y  -> VBool (x = y)
  | VChar   x, VChar  y   -> VBool (x = y)
  | VString x, VChar  y   -> VBool (x = (String.make 1 y))
  | VChar   x, VString y  -> VBool ((String.make 1 x) = y)
  | VBool b1, VBool b2    -> VBool (b1=b2)
  (* TODO: implement Date/Time/DateTime*)
  | _ -> failwith "VCEq ERROR" 

let vne v1 v2 = 
  match v1,v2 with 
  | VNull,_               -> VBool false
  | _,VNull               -> VBool false
  | VInt    x, VInt    y  -> VBool (x <> y)
  | VInt    x, VFloat  y  -> VBool ((float_of_int x) <> y)
  | VFloat  x, VInt    y  -> VBool (x <> (float_of_int y))
  | VFloat  x, VFloat  y  -> VBool (x <> y)
  | VString x, VString y  -> VBool (x <> y)
  | VChar   x, VChar  y   -> VBool (x <> y)
  | VString x, VChar  y   -> VBool (x <> (String.make 1 y))
  | VChar  x, VString y   -> VBool ((String.make 1 x) <> y)
  | VBool b1, VBool b2    -> VBool (b1<>b2)
  (* TODO: implement Date/Time/DateTime*)
  | _ -> failwith "VCNotEq ERROR"  

let vge v1 v2 = 
  match v1,v2 with 
  | VNull,_               -> VBool false
  | _,VNull               -> VBool false
  | VInt    x, VInt    y  -> VBool (x >= y)
  | VInt    x, VFloat  y  -> VBool ((float_of_int x) >= y)
  | VFloat  x, VInt    y  -> VBool (x >= (float_of_int y))
  | VFloat  x, VFloat  y  -> VBool (x >= y)
  | VString x, VString y  -> VBool (x >= y)
  | VChar   x, VChar   y  -> VBool (x >= y)
  | VString x, VChar   y  -> VBool (x >= (String.make 1 y))
  | VChar   x, VString y  -> VBool ((String.make 1 x) >= y)
  | VBool b1, VBool b2    -> VBool (b1>=b2)
  (* TODO: implement Date/Time/DateTime*)
  | _ -> failwith "VCNotEq ERROR"  

let vle v1 v2 = 
  match v1,v2 with 
  | VNull,_               -> VBool false
  | _,VNull               -> VBool false
  | VInt    x, VInt    y  -> VBool (x <= y)
  | VInt    x, VFloat  y  -> VBool ((float_of_int x) <= y)
  | VFloat  x, VInt    y  -> VBool (x <= (float_of_int y))
  | VFloat  x, VFloat  y  -> VBool (x <= y)
  | VString x, VString y  -> VBool (x <= y)
  | VChar   x, VChar   y  -> VBool (x <= y)
  | VString x, VChar   y  -> VBool (x <= (String.make 1 y))
  | VChar   x, VString y  -> VBool ((String.make 1 x) <= y)
  | VBool b1, VBool b2    -> VBool (b1<=b2)
  (* TODO: implement Date/Time/DateTime*)    
  | _ -> failwith "The expression is supposed to have type bool."

let vand v1 v2 = 
  match v1,v2 with 
  | VBool true,  VBool true  -> VBool true
  | VBool false, VBool true  -> VBool false 
  | VBool true,  VBool false -> VBool false 
  | VBool false, VBool false -> VBool false 
  | _ -> failwith "The expression is supposed to have type bool." 

let vor v1 v2 = 
  match v1,v2 with 
  | VBool true,  VBool true  -> VBool true
  | VBool false, VBool true  -> VBool true 
  | VBool true,  VBool false -> VBool true 
  | VBool false, VBool false -> VBool false 
  | _ -> failwith "The expression is supposed to have type bool." 

let vnot v1 = 
  match v1 with 
  | VBool b -> VBool (not b)
  | _ -> failwith "The expression is supposed to have type bool." 

let vconcat v1 v2 = 
  match v1,v2 with 
  | VString s1, VString s2 -> VString (s1^s2)
  | VString s1, VChar c1   -> VString (s1^(String.make 1 c1))
  | VChar c1,   VString s1 -> VString ((String.make 1 c1)^s1)
  | VChar c1,   VChar c2   -> VString ((String.make 1 c1)^
                                        (String.make 1 c2))
  | VNull,      VString s2 -> VString s2 
  | VNull,      VChar   c2 -> VString (String.make 1 c2)
  | VString s1, VNull      -> VString s1 
  | VChar   c1, VNull      -> VString (String.make 1 c1)
  | VNull     , VNull      -> VString ""
  | _ -> failwith "The expression is supposed to have type string /char."

(* ****************************************** *)
(* ************ helper functions ************ *)
let is_pos i = if i > 0 then true else false

let contains_str s1 s2 = 
  let substr = Str.regexp (".+"^s2^".+\\|.+"^s2^"$\\|^"^s2) in
  Str.string_match substr s1 0 

(* raise Not_found if s2 does not exist in s1 *)
(* return the index of the first occurrence of [s2] in [s1] *)
let substr_index_l_to_r s1 s2 = 
  let substr = Str.regexp s2 in
  Str.search_forward substr s1 0 

let substr_index_r_to_l s1 s2 = 
  let substr = Str.regexp s2 in
  (Str.search_backward substr s1 (String.length s1 - 1))+((String.length s2)-1)

let char_to_str c = String.make 1 c 

(* return the substring to the right of [s1] *)
(* when the input number is negative *)
let rec right_sub s1 s2 i = 
  let start = substr_index_r_to_l s1 s2 in
  let init = if start + 1 = String.length s1 
             then ""
             else String.sub s1 (start+1) (String.length s1 - (start+1)) 
  in
  if i = 1 then init
  else let ns1 = String.sub s1 0 start 
       in right_sub ns1 s2 (i-1)^
                           (String.sub s1 start ((String.length s1)-start))

(* when the input number is positive *)
let rec left_sub s1 s2 i = 
  let start = substr_index_l_to_r s1 s2 in
  let init = if start = 0 
             then ""
             else String.sub s1 0 start
  in 
  if i = 1 then init 
  else let ns1= String.sub s1 (start+1) (String.length s1 - (start+1)) 
       in (String.sub s1 0 (start+1))^left_sub ns1 s2 (i-1)

(* ************ helper functions ************ *)
(* ****************************************** *)

let vsubstr_ind v1 v2 v3 = 
  let s1 = match v1 with 
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 1st arg for 
                          SUBSTRING_INDEX should be either string or char."
  in
  let s2 = match v2 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 2nd arg for 
                          SUBSTRING_INDEX should be either string or char."
  in
  let i = match v3 with
           | VInt n -> Some n
           | VNull  -> None
           | _      -> failwith "Error: 3rd arg for 
                       SUBSTRING_INDEX should be int."
  in
  match s1, s2, i with
  | None,_,_ | _,None,_ | _,_,None -> VNull 
  | Some s1, Some s2, Some i       -> 
    if i = 0 then VString "" else
      if is_pos i then 
        if contains_str s1 s2 <> true then VString s1 
        else try VString (left_sub s1 s2 i) with Not_found -> VString s1
      else 
        if contains_str s1 s2 <> true then VString s1
        else try VString (right_sub s1 s2 (abs(i))) with Not_found ->VString s1

let vupper v1 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: arg for 
                          UPPER should be either string or char."
  in match s1 with
     | None -> VNull
     | Some s -> VString (String.uppercase_ascii s)

let vlower v1 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: arg for 
                          LOWER should be either string or char."
  in match s1 with
     | None -> VNull
     | Some s -> VString (String.lowercase_ascii s)

let vchar_length v1 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: arg for 
                          CHAR_LENGTH should be either string or char."
  in match s1 with
     | None -> VNull
     | Some s -> VInt (String.length s)

let vinsert v1 v2 v3 v4 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 1st arg for 
                          INSERT should be either string or char."
  in
  let n2 = match v2 with
           | VInt n -> Some n
           | VNull  -> None
           | _      -> failwith "Error: 2nd arg for 
                       INSERT should be int."
  in
  let n3 = match v3 with
           | VInt n -> Some n
           | VNull  -> None
           | _      -> failwith "Error: 3rd arg for 
                       INSERT should be int."
  in
  let s4 = match v4 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 4th arg for 
                          INSERT should be either string or char."
  in
  match s1,n2,n3,s4 with
  | None,_,_,_ | _,None,_,_ | _,_,None,_ | _,_,_,None -> VNull
  | Some str, Some pos, Some len, Some newstr         ->
    if pos = 0 || pos > String.length str then VString str else 
    let beforestr = String.sub str 0 (pos-1) in
    if len > String.length str - pos then VString (beforestr^newstr) else
    let poststr   = String.sub str (pos+len-1) (String.length str - (pos+len-1)) in
    VString (beforestr^newstr^poststr)

let vlocate v1 v2 v3 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 1st arg for 
                          LOCATE should be either string or char."
  in
  let s2 = match v2 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 2nd arg for 
                          LOCATE should be either string or char."
  in
  match s1, s2, v3 with
  | None,_,_ | _,None,_ | _,_,Some (VNull) -> VNull
  | Some substr, Some str, None            ->
    if contains_str str substr <> true then VInt 0 
    else VInt (substr_index_l_to_r str substr + 1)
  | Some substr, Some str, Some (VInt pos) ->
    if contains_str str substr <> true then VInt 0
    else
    let nstr = String.sub str (pos-1) (String.length str - pos + 1) in
    VInt ((pos-1) + substr_index_l_to_r nstr substr + 1)
  | _ -> failwith "Error: arg for LOCATE is invalid."

let vreverse v1 = 
  let s1 = match v1 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 1st arg for 
                          REVERSE should be either string or char."
  in match s1 with 
     | None -> VNull 
     | Some s -> 
       let cnt = ref ((String.length s)-1) in  
       let ret = ref "" in  
       while !cnt>=0 do  
         ret:=(!ret)^(String.sub s !cnt 1); 
         cnt:=!cnt-1 
       done; 
       VString (!ret) 

(* ******************************************** *)
(* ************* helper functions ************* *)
let trim_lead str sub = 
  if contains_str str sub <> true then str
  else
  let len = String.length sub in  
  let cnt = ref 0 in  
  let brk = ref false in 
  while (not !brk) && (!cnt+len)<String.length str do 
    if String.sub str !cnt len = sub then 
      cnt:= !cnt+len 
    else (brk := true)
  done;  
  String.sub str !cnt ((String.length str)-(!cnt))

let trim_trail str sub = 
  if contains_str str sub <> true then str
  else
  let len = String.length sub in
  let cnt = ref ((String.length str) - len) in
  let brk = ref false in
  while (not !brk) && (!cnt)>=0 do
    if String.sub str !cnt len = sub then(
      cnt:= !cnt-len;
    )
    else (
      brk := true;
        cnt := !cnt + len 
    )
  done;
  String.sub str 0 (!cnt)

let trim_space str = String.trim str

let trim_space_lead str = 
  let s1 = trim_lead str " " in
  let s2 = trim_lead s1 "\012" in
  let s3 = trim_lead s2 "\n" in
  let s4 = trim_lead s3 "\r" in
  let s5 = trim_lead s4 "\t" in s5

let trim_space_trail str =
  let s1 = trim_trail str " " in
  let s2 = trim_trail s1 "\012" in
  let s3 = trim_trail s2 "\n" in
  let s4 = trim_trail s3 "\r" in
  let s5 = trim_trail s4 "\t" in s5

(* ************* helper functions ************* *)
(* ******************************************** *)

let vtrim v1 v2 v3 = 
  let s3 = match v3 with
           | VString s -> Some s
           | VChar c   -> Some (char_to_str c)
           | VNull     -> None
           | _         -> failwith "Error: 3rd arg for 
                          TRIM should be either string or char."
  in 
  match v1, v2, s3 with
  | _,_,None | _,Some (VNull),_ -> VNull
  | Both    , Some (VString s)  , Some str -> let s1 = trim_lead str s in
                                              VString (trim_trail s1 s)
  | Both    , Some (VChar c)    , Some str -> let sub = char_to_str c in 
                                              let s1 = trim_lead str sub in 
                                              VString (trim_trail s1 sub)
  | Both    , None              , Some str -> VString (trim_space str)

  | Leading , Some (VString s)  , Some str -> VString (trim_lead str s)
  | Leading , Some (VChar c)    , Some str -> let sub = char_to_str c in 
                                              VString (trim_lead str sub)
  | Leading , None              , Some str -> VString (trim_space_lead str)

  | Trailing, Some (VString s)  , Some str -> VString (trim_trail str s)
  | Trailing, Some (VChar c)    , Some str -> let sub = char_to_str c in 
                                              VString (trim_trail str sub)
  | Trailing, None              , Some str -> VString (trim_space_trail str)
  | _ -> failwith "Error: arg for TRIM is invalid."

  (* [vcop_eval ast row] evaluates an vec_col_op ast on a single row *)
  let rec vcop_eval (ast:vec_col_op) (row:value array) : value = 
    try 
    match ast with 
    (* primitive types *)
    | VCDate     n  -> VDate     n
    | VCDateTime n  -> VDateTime n
    | VCTime     n  -> VTime     n
    | VCInt      n  -> VInt      n  
    | VCFloat    n  -> VFloat    n 
    | VCBool     n  -> VBool     n  
    | VCString   n  -> VString   n
    | VCChar     n  -> VChar     n  
    | VCNull        -> VNull
    (* direct access *)
    | VCNum i       -> row.(i) (* direct access *)
    | VCIsNull    e1 ->
      begin
        let v1 = vcop_eval e1 row in 
        if v1 = VNull then VBool true
        else VBool false  
      end
    | VCIsNotNull e1 ->
      begin
        let v1 = vcop_eval e1 row in 
        if v1 <> VNull then VBool true
        else VBool false  
      end
    (* plus *)
    | VCPlus(e1,e2) -> 
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in 
        vadd v1 v2 
        end 
    | VCMinus(e1,e2) -> 
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in 
        vminus v1 v2 
        end     
    | VCMult(e1,e2) ->
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in 
        vmult v1 v2 
        end         
    | VCDivi(e1,e2) ->
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in 
        vdivi v1 v2 
        end    
    | VCGt (e1,e2) ->
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        vgt v1 v2 
        end    
    | VCLt(e1,e2) -> 
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        vlt v1 v2 
        end   
    | VCEq(e1,e2) ->
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        veq v1 v2 
        end  
    | VCNotEq(e1,e2) -> 
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        vne v1 v2
        end      
    | VCGtEq(e1,e2) ->
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        vge v1 v2 
        end       
    | VCLtEq(e1,e2) -> 
        begin
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in  
        vle v1 v2 
        end  
    | VCAnd(e1,e2) ->
        begin          
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in
        vand v1 v2 
        end 

    | VCOr(e1,e2) -> 
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in
        vor v1 v2 
        end 
    | VCNot e1 -> 
        begin 
        let v1 = vcop_eval e1 row in 
        vnot v1 
        end 
    | VCMod(e1,e2) -> 
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in
        vmod v1 v2 
        end 
    | VCConcat(e1,e2) -> 
        begin 
        let v1 = vcop_eval e1 row in 
        let v2 = vcop_eval e2 row in
        vconcat v1 v2 
        end 
    | VCSubstr_Index (e1,e2,e3) ->
        begin
        let v1 = vcop_eval e1 row in
        let v2 = vcop_eval e2 row in
        let v3 = vcop_eval e3 row in
        vsubstr_ind v1 v2 v3
        end
    | VCUpper e1 ->
        begin
        let v1 = vcop_eval e1 row in
        vupper v1
        end
    | VCLower e1 ->
        begin
        let v1 = vcop_eval e1 row in
        vlower v1
        end
    | VCChar_length e1 ->
        begin
        let v1 = vcop_eval e1 row in
        vchar_length v1
        end
    | VCInsert (e1,e2,e3,e4) ->
        begin
        let v1 = vcop_eval e1 row in
        let v2 = vcop_eval e2 row in
        let v3 = vcop_eval e3 row in
        let v4 = vcop_eval e4 row in
        vinsert v1 v2 v3 v4
        end
    | VCLocate (e1,e2,e3) ->
        begin
        let v1 = vcop_eval e1 row in
        let v2 = vcop_eval e2 row in
        match e3 with 
        | Some e -> vlocate v1 v2 (Some (vcop_eval e row))
        | None   -> vlocate v1 v2 None
        end
    | VCTrim (e1,e2,e3) ->
        begin
        let v3 = vcop_eval e3 row in
        match e2 with
        | Some e -> vtrim e1 (Some (vcop_eval e row)) v3
        | None   -> vtrim e1 None v3
        end
    | VCReverse e1 ->
        begin
        let v1 = vcop_eval e1 row in
        vreverse v1
        end
    | _ -> failwith "[vcop_eval]: unimplemented."
    with Failure s -> failwith s 

  (* [gro_eval ast table (left,right)] evaluates an vec_col_op ast on a 
   * single group stating from [table.(left)] to [table.(right)] *)
  let rec grp_eval (ast:vec_col_op) (table:value array array) 
                   (left,right) : value = 
    try 
    match ast with 
    (* primitive types *)
    | VCDate     n  -> VDate     n
    | VCDateTime n  -> VDateTime n
    | VCTime     n  -> VTime     n
    | VCInt      n  -> VInt      n  
    | VCFloat    n  -> VFloat    n 
    | VCBool     n  -> VBool     n  
    | VCString   n  -> VString   n
    | VCChar     n  -> VChar     n  
    | VCNull        -> VNull
    (* direct access *)
    | VCNum i       -> table.(left).(i) (* direct access *)
    | VCIsNull    e1 ->
      begin
        let v1 = grp_eval e1 table (left,right) in 
        if v1 = VNull then VBool true
        else VBool false  
      end
    | VCIsNotNull e1 ->
      begin
        let v1 = grp_eval e1 table (left,right) in 
        if v1 <> VNull then VBool true
        else VBool false  
      end
    (* plus *)
    | VCPlus(e1,e2) -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in 
        vadd v1 v2
        end 
    | VCMinus(e1,e2) -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in 
        vminus v1 v2 
        end     
    | VCMult(e1,e2) ->
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in 
        vmult v1 v2 
        end         
    | VCDivi(e1,e2) ->
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in 
        vdivi v1 v2 
        end    
    | VCGt (e1,e2) ->
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        vgt v1 v2 
        end    
    | VCLt(e1,e2) -> 
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        vlt v1 v2  
        end   
    | VCEq(e1,e2) ->
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        veq v1 v2 
        end  
    | VCNotEq(e1,e2) -> 
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        vne v1 v2
        end      
    | VCGtEq(e1,e2) ->
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        vge v1 v2 
        end       
    | VCLtEq(e1,e2) -> 
        begin
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in  
        vle v1 v2
        end  
    | VCAnd(e1,e2) ->
        begin          
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in
        vand v1 v2 
        end 

    | VCOr(e1,e2) -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in
        vor v1 v2
        end 
    | VCNot e1 -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        vnot v1
        end 
    | VCMod(e1,e2) -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in
        vmod v1 v2
        end 
    | VCConcat(e1,e2) -> 
        begin 
        let v1 = grp_eval e1 table (left,right) in 
        let v2 = grp_eval e2 table (left,right) in
        vconcat v1 v2
        end 
    | VCSubstr_Index(e1,e2,e3) ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        let v2 = grp_eval e2 table (left,right) in
        let v3 = grp_eval e3 table (left,right) in
        vsubstr_ind v1 v2 v3
        end
    | VCUpper e1 ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        vupper v1
        end
    | VCLower e1 ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        vlower v1
        end
    | VCChar_length e1 ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        vchar_length v1
        end
    | VCInsert (e1,e2,e3,e4) ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        let v2 = grp_eval e2 table (left,right) in
        let v3 = grp_eval e3 table (left,right) in
        let v4 = grp_eval e4 table (left,right) in
        vinsert v1 v2 v3 v4
        end
    | VCLocate (e1,e2,e3) ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        let v2 = grp_eval e2 table (left,right) in
        match e3 with
        | Some e -> vlocate v1 v2 (Some (grp_eval e table (left,right)))
        | None   -> vlocate v1 v2 None
        end
    | VCTrim (e1,e2,e3) ->
        begin
        let v3 = grp_eval e3 table (left,right) in
        match e2 with
        | Some e -> vtrim e1 (Some (grp_eval e table (left,right))) v3
        | None   -> vtrim e1 None v3
        end
    | VCReverse e1 ->
        begin
        let v1 = grp_eval e1 table (left,right) in
        vreverse v1
        end
    | VCMax e1 ->
      begin
      let ret = ref VNull in   
      for i = left to right do 
        let cur_v = vcop_eval e1 table.(i) in 
        if (vgt !ret cur_v) = (VBool true) then ()
        else
          ret:= cur_v
      done; !ret 
      end 
    | VCMin e1 ->
      begin 
      let ret = ref VNull in   
      for i = left to right do 
        let cur_v = vcop_eval e1 table.(i) in 
        if (vlt !ret cur_v) = (VBool true) then ()
        else
          ret:= cur_v
      done; !ret
      end 
    | VCMed _ -> failwith "OSQL does not support median."
    | VCAvg e1 ->
      begin
      let ret = ref (VInt 0) in   
      for i = left to right do 
        let cur_v = vcop_eval e1 table.(i) in 
        ret:= (vadd !ret cur_v)         
      done; 
      (vdivi !ret (VInt(right-left+1))) 
      end 
    | VCSum e1 ->
      begin
      let ret = ref (VInt 0) in   
      for i = left to right do 
        let cur_v = vcop_eval e1 table.(i) in 
        ret:= (vadd !ret cur_v)         
      done; 
      !ret 
      end 
    | VCCount e1 -> 
      begin
      let ret = ref (VInt 0) in   
      for i = left to right do 
        let cur_v = vcop_eval e1 table.(i) in
          if cur_v <> VNull then 
            ret:= (vadd !ret (VInt 1)) 
          else ()         
      done; 
      !ret 
      end 
    with Failure s -> failwith s 