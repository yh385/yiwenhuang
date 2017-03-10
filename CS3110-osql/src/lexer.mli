open Ast
open Genlex

(* Convert the input character stream to a token stream. *)
val lex: char Stream.t -> token Stream.t