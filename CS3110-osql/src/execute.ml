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
 *  Drop certain column from a table     
val run : expr -> unit


(* [run_script] runs the osql code written in a file. 
 * The string input specifies the file name. *)
val run_script : string -> unit *)

let strval_lst set = List.fold_left (fun acc s -> 
                       let (ColName cn, v) = s in
                       (cn,v)::acc
                     ) [] set

let run exp = 
  match exp with 
  | CreateTable (IfNotExists exist,TName tname,CreateDef cdef,As tblas) -> 
    let attri_lst = List.fold_left (fun acc d -> 
                      match d with | Def ca -> ca::acc | PrimaryKey _ -> acc
                    ) [] cdef in
    let pkey = List.filter (fun d -> 
                 match d with | Def _ -> false | PrimaryKey _ -> true
               ) cdef in 
    (match pkey with 
    | [] -> 
      (match exist with
      | true  -> if Table.exist tname then () 
                 else 
                 begin
                   match tblas with
                   | Some (Select(sel_dist,cs_list, from_obj,where,
                                  group,having, order,limit_num)) -> 
                     let (ori_tbl,ali) = Table.from from_obj in 
                     let temptbl = (Table.select sel_dist cs_list (ori_tbl,ali) 
                                                where group having order limit_num)
                     in (Table.store_tbl (Table.create_as tname (List.rev attri_lst) temptbl));
                        print_endline ("Table "^tname^" is created.")
                   | None -> 
                      (Table.store_tbl 
                      (Table.create_empty tname (List.rev attri_lst) ""));
                       print_endline ("Table "^tname^" is created.")
                   | _ -> failwith "syntax error"
                 end 
      | false -> if Table.exist tname then failwith "This table already exist"
                 else 
                 begin
                   match tblas with
                   | Some (Select(sel_dist,cs_list, from_obj,where,
                                  group,having, order,limit_num)) -> 
                     let (ori_tbl,ali) = Table.from from_obj in 
                     let temptbl = (Table.select sel_dist cs_list (ori_tbl,ali) 
                                                where group having order limit_num)
                     in (Table.store_tbl (Table.create_as tname (List.rev attri_lst) temptbl));
                        print_endline ("Table "^tname^" is created.")
                   | None -> 
                      (Table.store_tbl 
                        (Table.create_empty tname (List.rev attri_lst) ""));
                      print_endline ("Table "^tname^" is created.")
                   | _ -> failwith "syntax error"
                 end )
    | [PrimaryKey k] -> 
      (match exist with
      | true  -> if Table.exist tname then () 
                 else 
                 begin
                   match tblas with
                   | Some (Select(sel_dist,cs_list, from_obj,where,
                                  group,having, order,limit_num)) -> 
                     let (ori_tbl,ali) = Table.from from_obj in 
                     let temptbl = (Table.select sel_dist cs_list (ori_tbl,ali) 
                                                where group having order limit_num)
                     in (Table.store_tbl (Table.create_as tname (List.rev attri_lst) temptbl));
                        print_endline ("Table "^tname^" is created.")
                   | None -> 
                      (Table.store_tbl 
                        (Table.create_empty tname (List.rev attri_lst) k));
                      print_endline ("Table "^tname^" is created.")
                   | _ -> failwith "syntax error"
                 end 
      | false -> if Table.exist tname then failwith "This table already exist"
                 else 
                 begin
                   match tblas with
                   | Some (Select(sel_dist,cs_list, from_obj,where,
                                  group,having, order,limit_num)) -> 
                     (let (ori_tbl,ali) = Table.from from_obj in 
                     let temptbl = (Table.select sel_dist cs_list (ori_tbl,ali) 
                                                where group having order limit_num)
                     in (Table.store_tbl (Table.create_as tname (List.rev attri_lst) temptbl));
                        print_endline ("Table "^tname^" is created."))
                   | None -> 
                        (Table.store_tbl 
                          (Table.create_empty tname (List.rev attri_lst) k));
                        print_endline ("Table "^tname^" is created.")
                   | _ -> failwith "Syntax error."
                 end )
      | _ -> failwith "Syntax error.")
  | InsertInto (TName tname, set) ->
    begin
    let temptbl = Table.load_tbl tname in
    let strvallst = strval_lst set in
    Table.insert_into temptbl (List.rev strvallst);
    Table.store_tbl temptbl
    end
  | Select(sel_dist,cs_list,
           from_obj,where,
           group,having,
           order,limit_num) -> 
      begin
        let (ori_tbl,ali) = Table.from from_obj in 
        let new_tbl = 
          Table.select sel_dist cs_list 
                       (ori_tbl,ali) 
                       where group having 
                       order limit_num in 
        Table.print_table new_tbl;
        print_endline ("Query completed.")
      end
  | Update(TName tname, cvlst, wh) ->
    begin
      let temptbl = Table.load_tbl tname in
      let strvallst = strval_lst cvlst in
      Table.update temptbl (List.rev strvallst) wh;
      Table.store_tbl temptbl;
      print_endline ("Table "^tname^" has been updated.")
    end
  | DeleteFrom(TName tname, wh) ->
    begin
      let temptbl = Table.load_tbl tname in
      Table.delete_from temptbl wh;
      Table.store_tbl temptbl;
      print_endline ("Table "^tname^" has been updated.")
    end
  | AlterTable(TName tname,alt) ->
    begin
      let temptbl = Table.load_tbl tname in
      let _ = 
      match alt with
      | AddCol (cn, ty)      -> Table.store_tbl (Table.add_col cn ty temptbl)
      | DropCol (ColName cn) -> Table.store_tbl (Table.drop_col cn temptbl) in
      print_endline ("A new column has been added to "^tname^".")
    end
  | DropTable(IfExists b,TName tname) ->
    begin
      if b then 
        if Sys.file_exists (Table.table_dir^tname^Table.table_postfix) 
        then( 
          Table.drop_table tname;
          print_endline ("Table "^tname^" has been deleted.")
        ) 
        else()
      else(
        Table.drop_table tname;
        print_endline ("Table "^tname^" has been deleted.")
      )
    end
  | TruncateTable (TName tname) ->
    begin
      let temptbl = Table.load_tbl tname in
      Table.truncate_tbl temptbl; 
      Table.store_tbl temptbl;
      print_endline ("Table "^tname^" has been truncated.")

    end
  | _ -> failwith "Unimplemented command." 

let file_dir = "."^Filename.dir_sep^
                 Table.db_dir^Filename.dir_sep^"query"^Filename.dir_sep 

let run_script fn = 
  let q_postfix = ".osql" in 
  let in_ch = Pervasives.open_in (file_dir^fn^q_postfix) in 
  let cmd_all = ref "" in 
  try 
  while true do
    let ln = Pervasives.input_line in_ch in 
    cmd_all:= (!cmd_all^ln) 
  done
  with _ -> (); 
  let r = Str.regexp ";" in 
  let lst = Str.split r !cmd_all in 
  let rec run_parse_str l = 
    match l with
    | [] -> ()
    | h::t -> run (parse_string h); run_parse_str t in 
              run_parse_str lst
