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
  val add_col : col_name  -> string -> t -> t
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
module Table : TableType