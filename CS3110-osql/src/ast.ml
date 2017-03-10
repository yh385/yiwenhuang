(* We will gradually add complexity as we progress. *)
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
type val_type = [`Int  | `Float    | `Char | `String | `Bool | 
                   `Date | `DateTime | `Time | `Null ]
type expr = 
  | Select of sel_dist * (col_op * sub_name) list * from_obj * where_cond
                                       * group_cond * hav_cond * order_cond * limit_num
  | CreateTable of exist * expr * create_def * tbl_as
  | InsertInto of expr * (col_name * value) list
  | DeleteFrom of expr * where_cond
  | Update of expr * ((col_name * value) list) * where_cond
  | AlterTable of expr * alt_tbl
  | DropTable of exist * expr
  | TruncateTable of expr
  | TName  of string  
(* col_op is a mini ast representing a series of operations on a column.
 * For instance, Plus(CName "money1"; CName "money2") means calculating the 
 * result of column `money1` + `money2`
 *)
and col_op = 
  | IsNull        of col_op
  | IsNotNull     of col_op
  | CPlus         of col_op * col_op 
  | CMinus        of col_op * col_op
  | CMult         of col_op * col_op   
  | CDivi         of col_op * col_op
  | CGt           of col_op * col_op    
  | CLt           of col_op * col_op 
  | CEq           of col_op * col_op 
  | CNotEq        of col_op * col_op 
  | CGtEq         of col_op * col_op 
  | CLtEq         of col_op * col_op 
  | CAnd          of col_op * col_op
  | COr           of col_op * col_op
  | CNot          of col_op  
  | CMod          of col_op * col_op 
  | CConcat       of col_op * col_op 
  | CSubstr_Index of col_op * col_op * col_op
  | CUpper        of col_op
  | CLower        of col_op
  | CChar_length  of col_op
  | CInsert       of col_op * col_op * col_op * col_op
  | CLocate       of col_op * col_op * col_op option
  | CTrim         of trim_obj * col_op option * col_op
  | CReverse      of col_op
  | CMax          of col_op  
  | CMin          of col_op  
  | CMed          of col_op (* not implemented *)
  | CAvg          of col_op 
  | CSum          of col_op
  | CCount        of col_op 
  | CName         of alias * string
  | CDate         of string  
  | CDateTime     of string  
  | CTime         of string  
  | CInt          of int     
  | CFloat        of float   
  | CBool         of bool    
  | CString       of string  
  | CChar         of char    
  | CNull
and alias = string option
and sub_name = string option
and sel_dist = Distinct of bool
and from_obj =   From of expr * sub_name * join_obj
and where_cond = Where of col_op option
and group_cond = GroupBy of col_op list * [`Asc | `Desc]
and hav_cond   = Having of col_op option
and order_cond = OrderBy of col_op list * [`Asc | `Desc]
and join_obj  = 
  | LJoin of expr * sub_name * join_cond 
  | RJoin of expr * sub_name * join_cond 
  | IJoin of expr * sub_name * join_cond 
  | NoJoin
and join_cond = On of col_op option  
and limit_num = Limit of int | NoLimit

(* types for CREATE TABLE *)
and create_def = CreateDef of col_def list
and col_name = ColName of string
and col_def = Def of col_attri | PrimaryKey of string 
and col_attri = {
  col_name  : string;
  data_type : string;
  not_null  : bool;
  default   : value;
  auto_incr : bool;
}
and tbl_as = As of expr option
and alt_tbl = AddCol of col_name * string | DropCol of col_name
and exist = IfNotExists of bool | IfExists of bool
and trim_obj = Both | Leading | Trailing