
(* Reading from csv file. *)
let in_ch = Pervasives.open_in "pkt_sniff.csv"
(* table names. *)
let tname1 = "pkt_addr"
let tname2 = "pkt_info"

(* drop tables if exists. *)
let init1   = "drop table if exists "^tname1
let init2   = "drop table if exists "^tname2

let create1 = (
  "create table if not exists "^tname1^"(
    id           int not null,
    time         float, 
    rx_addr      string,
    tx_addr      string,
    dst_addr     string
 ) ")

let create2 = (
  "create table if not exists "^tname2^"(
    id           int not null,
    src          string,
    dst          string,
    type_subtype string,
    protocal     string,
    length       int,   
    info         string
 ) ")


let rexp = Str.regexp "\",\""
let rmv_ht str = 
  let len = String.length str in 
  String.sub str 1 (len-3) 

let build () = 
  let _ = 
    run (parse_string init1);
    run (parse_string init2);
    run (parse_string create1);
    run (parse_string create2);
    Pervasives.input_line in_ch in  
  for _ = 0 to 1336 do 
    let line = rmv_ht (Pervasives.input_line in_ch)  in 
    let lst = (Str.split rexp line) in 
    let arr = Array.of_list lst in 
    if Array.length arr = 12 then( 
      let insert1 = ("
        INSERT INTO "^tname1^" 
           SET id           = "^arr.(0)^",
               time         = "^arr.(1)^",             
               rx_addr      = \""^arr.(2)^"\",
               tx_addr      = \""^arr.(3)^"\",
               dst_addr     = \""^arr.(4)^"\"" ) in
      let insert2 = ("
        INSERT INTO "^tname2^" 
           SET id           = "^arr.(0)^",
               src          = \""^arr.(5)^"\",
               dst          = \""^arr.(6)^"\",
               type_subtype = \""^arr.(7)^"\",
               protocal     = \""^arr.(9)^"\",
               length       = "^arr.(10)^""  ) in 
      run (parse_string insert1);
      run (parse_string insert2)
    )
    else ()
  done     
