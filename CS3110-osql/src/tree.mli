open vector 

type value = 
| VDate     of string
| VDateTime of string
| VTime     of string
| VInt      of int 
| VFloat    of float
| VBool     of bool 
| VString   of string 
| VChar     of char
| VNull     



module type BPTree = sig
  type 'key t 

  val empty  : 'key t 

  (* [insert k bptree] takes in a key and an original tree and inserts the key 
   * making sure the invariants are maintained. If the key already exists then
   * it is replaced. *)
  val insert : 'key -> 'key t -> 'key t

  (* [remove k bptree] takes in a key and an original tree and removes the key 
   * making sure the invariants are maintained. If the key does not exists then
   * this function does nothing. *)
  val remove : 'key -> 'key t -> 'key t

  type cmp_op =
  | Lt | LtEq | Eq | GtEq | Gt 

(* [compare v1 op v2] compares the keys with a given value and returns bool 
 * this can be used as a heper function with the search function. *)
  val compare : value -> cmp_op -> value -> bool 

  type 'a cond = 
  | CLt   of 'a
  | CLtEq of 'a
  | CEq   of 'a
  | CGtEq of 'a
  | CGt   of 'a
  | CNone
  
(* [search k_lst i_lst] takes in list of conditions as defined above and 
 * return a list of integers which are index to the vector. *)
  val search :  ('key cond) list -> int list

  (* [store_tree tr] will serialize [tr] to a binary file. *)
  val store_tree : 'key t -> t
  (* [load_tree file_name] wiil deserialize a tree from [file_name]. *)
  val load_tree  : string -> 'key t
end 

