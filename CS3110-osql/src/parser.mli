open Ast
open Lexer
open Genlex

(* Parse an expression into an SQL AST. *)
val parse_expr : token Stream.t -> expr

val parse_string : string -> expr 
(* A Helper function to peek what is inside a stream. 
 * This function should be removed when the implementation of 
 * parser is done. *)
val stream_to_list : 'a Stream.t -> 'a list