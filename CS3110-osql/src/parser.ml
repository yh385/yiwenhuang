open Ast
open Lexer
open Genlex

let rec check_order ls = 
  let is_pri_key p = 
    match p with
    | PrimaryKey _ -> true
    | _ -> false
  in
  match ls with
  | [] -> ()
  | [_] -> ()
  | h1::h2::t -> if is_pri_key h1 then failwith "Error_check_order" 
                                  else check_order (h2::t)

let check_value c = 
  match c with 
  | CDate d      -> VDate d 
  | CDateTime dt -> VDateTime dt 
  | CTime t      -> VTime t
  | CInt n       -> VInt n
  | CFloat f     -> VFloat f
  | CBool b      -> VBool b
  | CString s    -> VString s
  | CChar c      -> VChar c
  | CNull        -> VNull
  | _            -> failwith "Error_check_value"

let rec parse_expr = parser
  | [< 'Kwd "select"; 
         dt = parse_dist;
    sel_lst = (parse_col_op []); 
         fr = parse_from ?? "expected from"; 
         wh = parse_where; 
         gr = parse_group [];   
         hv = parse_hav;
        ord = parse_order []; 
         lm = parse_limit >] -> 
          let oe,os = ord in
          let ge,gs = gr in
          let fe,fs,fj = fr in 
          Select (dt, sel_lst, From (fe,fs,fj), Where wh, 
                  GroupBy (ge,gs), Having hv, OrderBy (oe,os), lm)

  | [< 'Kwd "create"; 'Kwd "table"; 
        ex = (parser | [<'Kwd "if"; 'Kwd "not"; 'Kwd "exists">] 
                             -> IfNotExists true 
                     | [< >] -> IfNotExists false); 
        tn=parse_tbl_name; tl >] ->
    (parser
      | [< 'Kwd "as"; cas=parse_cas >] 
                  -> CreateTable (ex, tn, CreateDef [], As cas)
      | [< 'Kwd "("; df=parse_def []; 'Kwd ")"; cas=parse_cas >] 
                  -> check_order df; CreateTable (ex, tn, CreateDef df, As cas)
    ) tl
  | [< 'Kwd "insert"; 'Kwd "into"; tn=parse_tbl_name; 
          'Kwd "set"; sc=parse_set [] >] -> InsertInto (tn, sc)
  | [< 'Kwd "delete"; 'Kwd "from"; tn=parse_tbl_name; wh=parse_where >] 
                                         -> DeleteFrom (tn, Where wh)
  | [< 'Kwd "update"; tn=parse_tbl_name; 'Kwd "set"; 
      sc=parse_set []; wh=parse_where >] -> Update (tn,sc,Where wh)
  | [< 'Kwd "alter"; 'Kwd "table"; tn=parse_tbl_name; alt=parse_alt_tbl >] 
                                         -> AlterTable (tn,alt)
  | [< 'Kwd "drop"; 'Kwd "table"; 
        ex = (parser | [<'Kwd "if"; 'Kwd "exists">] -> IfExists true 
                     | [< >]                        -> IfExists false); 
                  tn = parse_tbl_name >] -> DropTable (ex,tn)
  | [< 'Kwd "truncate"; 'Kwd "table"; tn=parse_tbl_name >] -> TruncateTable tn
  | [< 'Kwd "("; mid=parse_expr;     
       'Kwd ")";>]                         -> mid
  | [< 'Ident tn >]                        -> TName tn 

and parse_alt_tbl s = 
  match s with parser
  | [< 'Kwd "add"; 'Kwd "column"; 'Ident cname; 'Kwd dtype >] -> AddCol (ColName cname, dtype)
  | [< 'Kwd "drop"; 'Kwd "column"; 'Ident cname >] -> DropCol (ColName cname)
  | [< >] -> failwith "Error_parse_alt_tbl" 

and parse_set acc = parser
  | [< col = parse_colval; tl >] ->
    (parser
      | [< 'Kwd ","; tl1 = parse_set acc >] -> col::(tl1@acc)
      | [< >] -> col::acc
    ) tl
  | [< >] -> acc

and parse_colval s = 
  match s with parser
  | [< 'Ident cname; 'Kwd "="; value = parse_atom >] 
          -> (ColName cname, check_value value)
  | [< >] -> failwith "Error_parse_colval" 

and parse_cas s = 
  match s with parser
  | [< 'Kwd "as"; tl = parse_expr >] -> Some tl
  | [< tl = parse_expr >] -> Some tl
  | [< >] -> None

and parse_tbl_name s =
  match s with parser
  | [< 'Ident tn >] -> TName tn
  | [< >]           -> failwith "Error parse_tbl_name"

and parse_def acc = parser
  | [< col = parse_col; tl >] ->
    (parser
      | [< 'Kwd ","; tl1 = parse_def acc >] -> col::(tl1@acc)
      | [< >] -> col::acc
    ) tl
  | [< >] -> acc
and parse_col s =
 let parse_not_null = parser
  | [< 'Kwd "not"; 'Kwd "null" >] -> true 
  | [< >]                         -> false 
 in  
 let parse_default s =
  match s with parser
  | [< 'Kwd "default"; default=parse_atom >] -> (check_value default)
  | [< >] -> VNull
 in
 let parse_auto_incr = parser
  | [< 'Kwd "auto_increment" >] -> true 
  | [< >]                       -> false 
 in match s with parser
  | [< 'Ident cname; 'Kwd dtype; nn = parse_not_null; 
          df = parse_default; ai = parse_auto_incr >] 
          -> Def {col_name  = cname; 
                  data_type = dtype; 
                  not_null  = nn; 
                  default   = df; 
                  auto_incr = ai}
  | [< 'Kwd "primary"; 'Kwd "key"; 'Kwd "("; 'Ident pkey; 'Kwd ")" >] 
          -> PrimaryKey pkey
  | [< >] -> failwith "Error_parse_col"

and parse_dist s =
  match s with parser
  | [< 'Kwd "distinct" >] -> Distinct true
  | [< >] -> Distinct false

and parse_col_op acc = parser           
  | [< col0 = parse_or; sn = parse_subname; tl >] -> 
    (parser 
     | [< 'Kwd ","; tl1 = parse_col_op acc >] -> (col0,sn)::(tl1@acc) 
     | [< >] -> (col0,sn)::acc
    ) tl 

  | [< >] -> acc
and parse_from s =
  match s with parser
  | [< 'Kwd "from"; tl >] ->
    (parser
      | [< 'Kwd "("; e=parse_expr; sn=parse_subname; jn=parse_join; 'Kwd ")">]
         -> (e,sn,jn)
      | [< e=parse_expr; sn=parse_subname; jn=parse_join >] -> (e,sn,jn)
      | [< >] -> failwith "Error_from2"
    ) tl
  | [< >] -> failwith "Error_from"

and parse_where s = 
  match s with parser
  | [< 'Kwd "where"; tail >] -> 
    (parser
    | [< e=parse_or >] -> Some e 
    | [<   >] -> failwith "where what???"
    ) tail
  | [< >] -> None 

and parse_group acc s = 
  let rec group_help acc = parser
    | [< col0 = parse_or; tl >] ->
      (parser
        | [< 'Kwd ","; tl1 = group_help acc >] -> col0::(tl1@acc)
        | [< >] -> col0::acc
      ) tl
    | [< >] -> acc in
  match s with parser
  | [< 'Kwd "group"; 'Kwd "by"; tail=group_help []; flag >]
      -> (parser
         | [< 'Kwd "asc" >] -> (tail,`Asc )
         | [< 'Kwd "desc">] -> (tail,`Desc)
         | [< >]            -> (tail,`Asc ) 
          ) flag 
  | [< >] -> (acc,`Asc)

and parse_hav s =
  match s with parser
  | [< 'Kwd "having"; tail >] ->
    (parser
    | [< e=parse_or >] -> Some e
    | [< >] -> failwith "Error_having"
    ) tail
  | [< >] -> None

and parse_order acc s =   
  let rec order_help acc =parser
    | [< col0 = parse_or; tl >] -> 
      (parser 
       | [< 'Kwd ","; tl1 = order_help acc >] -> col0::(tl1@acc) 
       | [< >] -> col0::acc
      ) tl 
    | [< >] -> acc in 
  match s with parser 
  | [< 'Kwd "order"; 'Kwd "by" ?? "where is by??"; tail=order_help []; flag >] 
      -> (parser 
         | [< 'Kwd "asc" >] -> (tail,`Asc )
         | [< 'Kwd "desc">] -> (tail,`Desc)
         | [< >]            -> (tail,`Asc ) 
          ) flag 
  | [< >] -> (acc,`Asc)

and parse_join s = 
  match s with parser 
  | [< 'Kwd "left";  'Kwd "join"; e=parse_expr;
        sn = parse_subname; cond=parse_on >] -> LJoin (e,sn,On cond)  
  | [< 'Kwd "right"; 'Kwd "join"; e=parse_expr;
        sn = parse_subname; cond=parse_on >] -> RJoin (e,sn,On cond) 
  | [< 'Kwd "inner"; 'Kwd "join"; e=parse_expr;
        sn = parse_subname; cond=parse_on >] -> IJoin (e,sn,On cond) 
  | [<                                              >] -> NoJoin
and parse_on s = 
  match s with parser
  | [< 'Kwd "on"; tail >] -> 
    (parser
    | [< e=parse_or >] -> Some e 
    | [<   >] -> failwith "on what???"
    ) tail
  | [< >] -> None 

and parse_limit s =
  match s with parser
  | [< 'Kwd "limit"; 'Int n >] -> Limit n
  | [< >] -> NoLimit

and parse_trim s = 
  match s with parser
  | [< 'Kwd "both" >]     -> Both
  | [< 'Kwd "leading" >]  -> Leading
  | [< 'Kwd "trailing" >] -> Trailing

and parse_or s = 
  match s with parser
  | [< cond = parse_and; tl >] ->
    (parser
     | [< 'Kwd "or";  tl1 = parse_or >] -> COr(cond,tl1)
     | [< >] -> cond
    ) tl 
and parse_and s = 
  match s with parser
  | [< cond = parse_not; tl >] ->
    (parser
     | [< 'Kwd "and";  tl1 = parse_and >] -> CAnd(cond,tl1)
     | [< >] -> cond
    ) tl 
and parse_not s = 
  match s with parser
  | [< 'Kwd "not";  tl1 = parse_not >] -> CNot tl1
  | [< e = parse_factor >] -> e 

and parse_factor s = 
  match s with parser 
  | [< col0 = parse_factor_high; tl >] ->
    (parser
     | [< 'Kwd "<";  tl1 = parse_factor >] -> CLt   (col0,tl1)
     | [< 'Kwd "<="; tl1 = parse_factor >] -> CLtEq (col0,tl1)
     | [< 'Kwd "=";  tl1 = parse_factor >] -> CEq   (col0,tl1)
     | [< 'Kwd "<>"; tl1 = parse_factor >] -> CNotEq(col0,tl1)
     | [< 'Kwd ">="; tl1 = parse_factor >] -> CGtEq (col0,tl1)
     | [< 'Kwd ">";  tl1 = parse_factor >] -> CGt   (col0,tl1)
     | [< >] -> col0
    ) tl
and parse_factor_high s = 
  match s with parser 
  | [< col0 = parse_factor_mid; tl >] -> 
    (parser
     | [< 'Kwd "+"; tl1 = parse_factor_high >] -> CPlus (col0,tl1)
     | [< 'Kwd "-"; tl1 = parse_factor_high >] -> CMinus(col0,tl1)
     | [< >] -> col0
    ) tl
and parse_factor_mid s = 
  match s with parser 
  | [< col0 = parse_factor_low; tl >] -> 
    (parser
     | [< 'Kwd "*"; tl1 = parse_factor_mid >] -> CMult(col0,tl1)
     | [< 'Kwd "/"; tl1 = parse_factor_mid >] -> CDivi(col0,tl1)
     | [< >] -> col0
    ) tl
and parse_factor_low s = 
  match s with parser 
  | [< 'Kwd "concat"; 'Kwd "("; fst = parse_factor; 'Kwd ","; 
        snd = parse_factor; 'Kwd ")" >] -> CConcat(fst,snd)
  | [< 'Kwd "substring_index"; 'Kwd "("; str=parse_or; 'Kwd ","; delim=parse_or; 
       'Kwd ","; count=parse_or; 'Kwd ")" >]  -> CSubstr_Index (str,delim,count)
  | [< 'Kwd "upper"; 'Kwd "("; str = parse_or; 'Kwd ")" >]      -> CUpper str
  | [< 'Kwd "lower"; 'Kwd "("; str = parse_or; 'Kwd ")" >]      -> CLower str
  | [< 'Kwd "char_length"; 'Kwd "("; str = parse_or; 'Kwd ")">] -> CChar_length str
  | [< 'Kwd "insert"; 'Kwd "("; str = parse_or; 'Kwd ","; pos = parse_or; 'Kwd ","; 
        len = parse_or; 'Kwd ","; newstr = parse_or; 'Kwd ")" >] -> CInsert (str,pos,len,newstr)
  | [< 'Kwd "locate"; 'Kwd "("; substr = parse_or; 'Kwd ","; str = parse_or; tl >] ->
    (parser
      | [< 'Kwd ","; pos = parse_or; 'Kwd ")" >] -> CLocate (substr, str, Some pos)
      | [< 'Kwd ")" >]                 -> CLocate (substr, str, None)
    )tl
  | [< 'Kwd "trim"; 'Kwd "("; trimobj = parse_trim; tl >] ->
    (parser 
      | [< remstr = parse_or; 'Kwd "from"; str = parse_or; 'Kwd ")">] -> CTrim (trimobj, Some remstr, str)
      | [< 'Kwd "from"; str = parse_or; 'Kwd ")" >] -> CTrim (trimobj, None, str)
    ) tl
  | [< 'Kwd "reverse"; 'Kwd "("; str = parse_or; 'Kwd ")" >]    -> CReverse str
  | [< 'Kwd "max";    'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CMax mid 
  | [< 'Kwd "min";    'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CMin mid 
  | [< 'Kwd "avg";    'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CAvg mid 
  | [< 'Kwd "median"; 'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CMed mid
  | [< 'Kwd "sum";    'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CSum mid
  | [< 'Kwd "count";  'Kwd "("; mid = parse_factor; 'Kwd ")" >] -> CCount mid   
  | [< col0 = parse_atom ; tl >] -> 
    (parser
     | [< 'Kwd "%"; tl1 = parse_factor_low >] -> CMod(col0,tl1)
     | [< >] -> col0
    ) tl
and parse_atom s = 
  match s with parser 
  | [< 'Kwd "*" >]      -> CName (None, "*")
  | [< 'Ident id; tl >] -> 
        (parser
        | [< 'Kwd "."; col >] ->
          (parser 
           | [< 'Ident n; null >] -> 
             (parser 
              | [<'Kwd "is"; 'Kwd "null" >] -> IsNull (CName (Some id, n))
              | [<'Kwd "not"; 'Kwd "null" >] -> IsNotNull (CName (Some id, n))
              | [< >] -> CName (Some id, n)
             ) null
           | [< 'Kwd "*"; null >] -> 
             (parser 
              | [<'Kwd "is"; 'Kwd "null" >] -> IsNull (CName (Some id, "*"))
              | [<'Kwd "not"; 'Kwd "null" >] -> IsNotNull (CName (Some id, "*"))
              | [< >] -> CName (Some id, "*")
             ) null
          ) col
        | [<'Kwd "is"; 'Kwd "null" >]             -> IsNull (CName (None, id ))
        | [<'Kwd "not"; 'Kwd "null" >] -> IsNotNull (CName (None, id ))
        | [< >]                                   -> CName (None, id )
        ) tl 
  | [< 'Kwd "("; mid = parse_or; 'Kwd ")" >] -> mid 
  | [< 'Int   n >]   -> CInt    n 
  | [< 'Kwd "-"; 'Int n>] -> CInt (-n)
  | [< 'Float n >]   -> CFloat  n
  | [< 'String s>]   -> CString s
  | [< 'Char  c >]   -> CChar   c 
  | [< 'Kwd "true">] -> CBool   true 
  | [< 'Kwd "false">]-> CBool   false  
  | [< 'Kwd "null" >]-> CNull
(* TODO: Date/Time/DateTime*)
and parse_subname s = 
  match s with parser  
  | [< 'Kwd "as"; 'Ident sn >] -> Some sn
  | [<  >]                     -> None   
  




let parse_string s = 
  s |> Stream.of_string |> lex |> parse_expr

let stream_to_list s = 
  let end_of_stream = ref false in 
  let ret = ref [] in 
  let _ = 
  while not !end_of_stream do 
    try 
    ret:= (Stream.next s)::(!ret)
    with _ -> end_of_stream:= true 
  done
  in List.rev !ret


