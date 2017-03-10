open Ast
open Genlex

let keyop    = ["("; ")";  "`";
                "+"; "-";  "*";  "/"; "%"; 
                ">"; "<"; ">="; "<="; "="; "<>";
                ","; "."; 
                "and"; "or"; "not" ]
let keyval   = ["true"; "false"; "null"; "int"; "string"; "char"; "float"]
let keyident = ["select"; "distinct"; "from"; "as"; "where"; "order"; "group"; "by"; 
                "having"; "left"; "right"; "inner"; "join"; "on"; "limit"]
let keyident2 = ["create"; "table"; "primary"; "key"; "default"; "auto_increment" ]
let keyident3 = ["insert"; "into"; "set"; "delete"; "update"; "alter"; "add"; "drop"; 
                 "column"; "if"; "exists"; "is"; "truncate"]
let keyident4 = ["substring_index"; "upper"; "lower"; "char_length"; "insert"; 
                 "locate"; "trim"; "both"; "leading"; "trailing"; "reverse"]
let keyflag  = ["asc"; "desc"] 
let keyfunc  = ["max"; "min"; "avg"; "sum"; "concat"; "count"]
let keywords = keyident@keyfunc@keyop@keyval@keyflag@keyident2@keyident3@keyident4

(* [is_keyword] is a helper function that checks if the input string is 
 * in the keyword list. It is case-insensitive. *)
let is_keyword s = 
  let tmp = String.lowercase_ascii s in 
  if List.mem tmp keywords then true else false 

let lex stream =
    let rec lex_help = parser
      | [< 'Ident s when is_keyword s; t=lex_help >] 
          -> [< 'Kwd (String.lowercase_ascii s) ; t >] 
      | [< 'Int n when n<0; t=lex_help >] 
          -> [< 'Kwd "-"; 'Int (-n); t >]
      | [< 'h; t = lex_help >]        -> [< 'h; t >]                 
      | [< >] -> [< >] in
  lex_help(make_lexer keywords stream)
