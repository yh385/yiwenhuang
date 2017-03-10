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
    method push_back : value array -> unit
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

module Vector : RowVector