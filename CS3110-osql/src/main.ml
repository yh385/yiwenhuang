open Core_extended.Readline
open Ast
open Lexer
open Parser
open VecAst
open Vector
open Table
open Execute

let has_semi s =
  let r = Str.regexp ".+;.+\\|.+;$\\|^;" in Str.string_match r s 0

let str_of s =
  match s with
  | Some a -> a
  | None   -> ""

let create_dir () =
  if Sys.file_exists Table.db_dir then ()
  else(
    Unix.mkdir Table.db_dir 0o777 
  );
  if Sys.file_exists Table.table_dir then ()
  else(
    Unix.mkdir Table.table_dir 0o777 
  );
  if Sys.file_exists Execute.file_dir then ()
  else(
    Unix.mkdir Execute.file_dir 0o777
  )
(*---------------------------------------*)
(*           Error Messages              *)
(*---------------------------------------*)
let syn_err = "Syntax error. Please check your osql syntax."
let unknown_error = 
  "An error occurs while running input command. Please try again."


let _ =
  create_dir ();
  let quit = ref false in 
  let reg_semi = Str.regexp ";" in 
  (* Welcome Message *)
  print_endline "\n\nWelcome to the OSQL monitor.  Commands end with ;";
  print_endline "Type quit; to exit.  \n";
  (* Interpreting user input. *)
  while not !quit do  
    let encount_semi = ref false in 
    let cmd  = ref "" in 
    let prpt = ref "osql>" in
    (* keep reading commands until seeing ";" .*)
    while not !encount_semi do
      let newline = str_of (input_line ~prompt:!prpt () ) in
      if not (has_semi newline) then
        cmd:= (!cmd^(newline))
      else
        (cmd:= !cmd^( if (Str.split reg_semi newline) = [] then ""
                      else List.hd (Str.split reg_semi newline));
        encount_semi:= true );
      prpt:="  >>>"
    done; 
    if (String.trim !cmd) = "quit" then (* TODO: improve *) 
      quit:= true 
    else if !cmd = "" then (print_string "\n") 
    else( 
      let r = Str.regexp " +" in 
      let kl = Str.split r !cmd in 
      if List.length kl = 2 then(
        let sr,fn = 
          match kl with 
          | h1::h2::[]  -> (h1,h2)
          | _ -> failwith "Impossible" in 
        if sr = "source" then 
          try run_script fn with 
          | _ -> print_endline ("Query "^fn^" has syntax error.")
        else 
          print_endline "Invalid command."
      )
      else
      (try 
        !cmd |> parse_string |> run
      with 
      | Failure s -> print_endline s 
      | Stream.Error _ -> print_endline syn_err
      (* | _ -> print_endline unknown_error *));
    print_endline "\n")
  done