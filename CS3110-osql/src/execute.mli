open Ast
open Table
open Parser


(* [run] will execute the AST produced by the interpreter. 
 * It has the following functions:
 * 
 *  Display query result
 *  Create a new table
 *  Update data in a table
 *  Insert a new record into a table
 *  Remove certain record(s) from a table 
 *  Add a new column to an existing table
 *  Drop certain column from a table      *)
val run : expr -> unit

val file_dir : string
(* [run_script] runs the osql code written in a file. 
 * The string input specifies the file name. *)
val run_script : string -> unit

(* Possible Helper functions: *)
(* val create_table : expr -> unit  *)
(* val select   :     expr -> unit  *)
(* val insert   :     expr -> unit  *)
(* val remove   :     expr -> unit  *)
(* val add_col  :     expr -> unit  *)
(* val drop_col :     expr -> unit  *)
