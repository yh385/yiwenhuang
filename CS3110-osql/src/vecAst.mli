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
  (* SUBSTRING_INDEX(str,delim,count) -> string * string * int *)
  (* if [delim] doesn't exist, return the entire [str] *)
  | VCSubstr_Index of vec_col_op * vec_col_op * vec_col_op
  (* UPPER(str) -> string *)
  | VCUpper        of vec_col_op
  (* LOWER(str) -> string *)
  | VCLower        of vec_col_op
  (* CHAR_LENGTH(str) -> string *)
  | VCChar_length  of vec_col_op
  (* INSERT(str,pos,len,newstr) -> string * int * int * string *)
  | VCInsert       of vec_col_op * vec_col_op * vec_col_op * vec_col_op
  (* LOCATE(substr, str) OR LOCATE(substr, str, pos) -> string * string * int option *)
  | VCLocate       of vec_col_op * vec_col_op * vec_col_op option
  (* TRIM([{BOTH | LEADING | TRAILING} [remstr] FROM] str) *)
  (* trim_obj * string option * string *)
  | VCTrim         of trim_obj * vec_col_op option * vec_col_op
  (* REVERSE(str) -> string *)
  | VCReverse       of vec_col_op
  | VCMax      of vec_col_op  
  | VCMin      of vec_col_op  
  | VCMed      of vec_col_op 
  | VCAvg      of vec_col_op 
  | VCSum      of vec_col_op 
  | VCCount    of vec_col_op
  | VCNum      of int        (* column number *)
  | VCDate     of string  
  | VCDateTime of string  
  | VCTime     of string  
  | VCInt      of int     
  | VCFloat    of float   
  | VCBool     of bool    
  | VCString   of string  
  | VCChar     of char    
  | VCNull   

(* arithmetic functions *)
val vadd : value -> value -> value 
val vminus : value -> value -> value 
val vmult : value -> value -> value 
val vdivi : value -> value -> value 
val vmod : value -> value -> value 
val vlt : value -> value -> value 
val vgt : value -> value -> value 
val veq : value -> value -> value 
val vne : value -> value -> value 
val vge : value -> value -> value 
val vle : value -> value -> value 
val vand : value -> value -> value 
val vor : value -> value -> value 
val vnot : value -> value
val vconcat : value -> value -> value 
val vsubstr_ind : value -> value -> value -> value
val vupper : value -> value
val vlower : value -> value 
val vchar_length : value -> value
val vinsert : value -> value -> value -> value -> value
val vlocate : value -> value -> value option -> value
val vreverse : value -> value
val vtrim : trim_obj -> value option -> value -> value 

val vcop_eval : vec_col_op -> value array -> value
val grp_eval :  vec_col_op -> value array array -> int * int -> value 