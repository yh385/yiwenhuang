open OUnit2
open Core_extended.Readline
open Ast
open Lexer
open Parser
open VecAst
open Vector
open Table
open Execute

let empty_vector = Vector.make_empty ()

let vector_tests = "vector class" >::: [
  "empty" >:: (fun _ -> assert_equal [||] (empty_vector#to_array));
  "pushback1" >:: (fun _ -> assert_equal [|[|VInt 0; VString "zero"|]|]
    ((empty_vector#push_back [|VInt 0; VString "zero"|]); empty_vector#to_array));
  "pushback2"   >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |]|]
    ((empty_vector#push_back [|VInt 1; VNull |]); empty_vector#to_array));
  "pushback3"   >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |];[|VInt 1; VInt 1|]|]
    ((empty_vector#push_back [|VInt 1; VInt 1|]); empty_vector#to_array));
  "pushback4"   >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |];[|VInt 1; VInt 1|];[|VInt 1; VString "one"; VNull |]|]
    ((empty_vector#push_back [|VInt 1; VString "one"; VNull |]); empty_vector#to_array));
  "pushback5" >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |];[|VInt 1; VInt 1|];[|VInt 1; VString "one"; VNull |];[|VInt 1; VString "one"; VNull |]|]
    ((empty_vector#push_back [|VInt 1; VString "one"; VNull |]); empty_vector#to_array));
  "eliminate_dups" >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |];[|VInt 1; VInt 1|];[|VInt 1; VString "one"; VNull |]|]
    (empty_vector#eliminate_dups; empty_vector#to_array));
  "pushback6" >:: (fun _ -> assert_equal
    [|[|VInt 0; VString "zero"|];[|VInt 1; VNull |];[|VInt 1; VInt 1|];[|VInt 1; VString "one"; VNull |];[|VInt 1 |]|]
    ((empty_vector#push_back [|VInt 1 |]); empty_vector#eliminate_dups; empty_vector#to_array));
  "truncate" >:: (fun _ -> assert_equal [||] (empty_vector#truncate; empty_vector#to_array));
  "pushback7" >:: (fun _ -> assert_equal [|[|VInt 0; VInt (0) |];[|VInt 1; VInt (2) |];[|VInt 2; VInt (4) |]; [|VInt 3; VInt (6)|]|]
    (
      for i = 0 to 3 do
        empty_vector#push_back [|VInt i; VInt (i*2) |]
      done; empty_vector#to_array
    ));
  "make_array1" >:: (fun_ -> assert_equal
    [|[|VInt 0; VInt (0) |];[|VInt 1; VInt (2) |];[|VInt 2; VInt (4) |]; [|VInt 3; VInt (6)|]|]
    (let c = Vector.make_array (empty_vector#to_array) in c#to_array));
  "add_col" >:: (fun _ -> assert_equal
    [|[|VInt 0; VInt (0); VNull|];[|VInt 1; VInt (2);VNull |];[|VInt 2; VInt (4);VNull|]; [|VInt 3; VInt (6);VNull|]|]
    (empty_vector#add_col; empty_vector#to_array));
  "at0" >:: (fun _ -> assert_equal
    [|[|VInt 0; VInt (0); VNull|]|]
    (empty_vector#at 0));
  "next_val1" >:: (fun _ -> assert_equal
    VInt 7
    (empty_vector#next_val 1));
  "next_val0" >:: (fun _ -> assert_equal
    VInt 4
    (empty_vector#next_val 0));
  "drop_col2" >:: (fun _ -> assert_equal
    [|[|VInt 0; VInt (0) |];[|VInt 1; VInt (2) |];[|VInt 2; VInt (4) |]; [|VInt 3; VInt (6)|]|]
    (empty_vector#drop_col 2));
  "drop_col0" >:: (fun _ -> assert_equal
    [|[|VInt (0) |];[|VInt (2) |];[|VInt (4) |]; [|VInt (6)|]|]
    (empty_vector#drop_col 0));
  "set_row" >:: (fun _ -> assert_equal
    [|[|VInt 1|];[|VInt 1; VInt (2) |];[|VInt 2; VInt (4) |]; [|VInt 3; VInt (6)|]|]
    (empty_vector#set_row [|VInt 1|] 0; empty_vector#to_array));
  "add_col" >:: (fun _ -> assert_equal
    [|[|VInt 1; VNull|];[|VInt 1; VInt (2); VNull |];[|VInt 2; VInt (4); VNull |]; [|VInt 3; VInt (6); VNull|]|]
    (empty_vector#add_col; empty_vector#to_array));
  "size" >:: (fun _ -> assert_equal 4 empty_vector#size);
]

let table_tests = "parser test" > :::
[
let a = {col_name = "name"; data_type = "int";
 not_null = true; default = VInt 1;
 auto_incr = false}

let b = {dtype = `Int; not_null = true ; default = VInt 1 ; auto_incr = false}

let c = new vec Empty

let d = {table_name = "y"; col_attribute = [|[|("name", b)|]|] ; indexed_col = ""; data_vec = c }

let e = {dtype = `Int; not_null = false; default = VNull ; auto_incr = false}

let f = {table_name = "y"; col_attribute = [|[|("name", b)|];[|("type", e)|]|] ; indexed_col = ""; data_vec = [|[|VNull|]] }

let g = {table_name = "y"; col_attribute = [|[|("type", e)|]|] ; indexed_col = ""; data_vec = [|[|VNull|]] }

let h = {table_name = "tbl1"; col_attribute = [|[|("type", e)|]|] ; indexed_col = []; data_vec = [|[|VNull|]] }

let i = {table_name = "tbl2"; col_attribute = [|[|("name", b)|];[|("type", e)|]|] ; indexed_col = ""; data_vec = [|[|VNull|]] }

let j = {table_name = "tbl2"; col_attribute = [|[||]|] ; indexed_col = ""; data_vec = [|[|VNull|]] }

let k = {table_name = "tbl3"; col_attribute = [|[|("type", e)|]|] ; indexed_col = []; data_vec = [|[|VNull|]] }

let l = {table_name = "tbl3"; col_attribute = [|[|("type", e)|]|] ; indexed_col = []; data_vec = [|[|VInt 1|]] }



"create_empty" >:: (fun _ -> assert_equal ({table_name = "y"; col_attribute = [|("name", b)|] ; indexed_col = ""; data_vec = c }) (create_empty("y" [a] "" )));
(*val add_col : col_name -> string -> t -> t*)
"add_col" >:: (fun _ -> assert_equal (
      {table_name = "y"; col_attribute = [|[|("name", b)|];[|("type", e)|]|] ; indexed_col = ""; data_vec = [|[|VNull|]] })
        (add_col (ColName "type"  "int" d)));
(* val drop_col : string -> t -> t *)
"drop_col" >:: (fun _ -> assert_equal (
      g
        (drop_col ("name"  f)));
(*val create_as    : string -> col_attri list -> t -> t *)
"create_as" >:: (fun _ -> assert_equal (
       h
        (create_as ("tbl1" , g)));
(*val truncate_tbl : t -> unit *)
"truncate" >:: (fun _ -> assert_equal (
       j
        ((truncate i ); load_tbl ("tbl2"));
(*val insert_into : t -> (string * value) list -> unit*)
"insert_into" >:: (fun _ -> assert_equal (
       l
        ((insert_into [("type", VInt 1)] k); load_tbl ("tbl3"));
(*val update : t -> (string * value) list -> where_cond -> unit*)
(*val exist : string -> bool *)
"exist" >:: (fun _ -> assert_equal (
       true
        (exist tbl1)));
(* val drop_table   : string -> unit *)
"drop_table" >:: (fun _ -> assert_equal (
       false
        (drop_table "tlb3"; exist "tbl3")));

(*val get_vec  : t -> vector*)
"get_vec" >:: (fun _ -> assert_equal (
       [|[|VNull|]]
        (get_vec g)));
(* val raw_data : t -> value array array*)
"raw_data" >:: (fun _ -> assert_equal (
       [|[|VNull|]]
        (get_vec g)))
]

let _ = run_test_tt_main vector_tests@table_tests

(* let test = Table.create_empty "test" [{col_name = "a"; data_type = "string"; not_null = true; default = "VInt 0"}; auto_incr = false};
                                      ] ;;

create table personal_info (id int, name string, major string, grade string);

insert into personal_info set id = 0, name = "Mary Ji", major = "Computer Science", grade = "Sophomore";
insert into personal_info set id = 1, name = "Yanghui Ou", major = "ECE", grade = "Junior";
insert into personal_info set id = 4, name = "Yiwen Huang", major = "Computer Science", grade = "Sophomore";
insert into personal_info set id = 2, name = "Nina G", major = "Computer Science", grade = "Junior";
insert into personal_info set id = 3, name = "Michael Clarkson", major = "Computer Science", grade = "Professor";

select * from personal_info order by id desc; *)
