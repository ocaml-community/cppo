%{
  open Cppo_types
%}

/* Directives */
%token < Cppo_types.loc * string > UNDEF INCLUDE WARNING ERROR
%token < Cppo_types.loc * string * (string * Cppo_types.shape) list > DEF
%token < Cppo_types.loc * string option * int > LINE
%token < Cppo_types.loc * Cppo_types.bool_expr > IFDEF
%token < Cppo_types.loc * string * string > EXT
%token < Cppo_types.loc > ENDEF SCOPE ENDSCOPE IF ELIF ELSE ENDIF ENDTEST

/* Boolean expressions in #if/#elif directives */
%token TRUE FALSE DEFINED NOT AND OR EQ LT GT NE LE GE
       PLUS MINUS STAR LNOT LSL LSR ASR LAND LOR LXOR
%token < Cppo_types.loc > OP_PAREN SLASH MOD
%token < int64 > INT


/* Regular program and shared terminals */
%token < Cppo_types.loc > CL_PAREN COMMA CURRENT_LINE CURRENT_FILE
%token < Cppo_types.loc * string > IDENT FUNIDENT
%token < Cppo_types.loc * bool * string > TEXT /* bool means "is space" */
%token EOF

/* Priorities for boolean expressions */
%left OR
%left AND

/* Priorities for arithmetics */
%left PLUS MINUS
%left STAR SLASH
%left MOD LSL LSR ASR LAND LOR LXOR
%nonassoc NOT
%nonassoc LNOT
%nonassoc UMINUS

%start main
%type < Cppo_types.node list > main
%%

main:
| unode main { $1 :: $2 }
| EOF        { [] }
;

unode_list0:
| unode unode_list0  { $1 :: $2 }
|                    { [] }
;

body:
| unode_list0
    { let pos1 = Parsing.symbol_start_pos()
      and pos2 = Parsing.symbol_end_pos() in
      let loc = (pos1, pos2) in
      (loc, $1) }

pnode_list0:
| pnode pnode_list0  { $1 :: $2 }
|                    { [] }
;

actual:
| pnode_list0        { let pos1 = Parsing.symbol_start_pos()
                       and pos2 = Parsing.symbol_end_pos() in
                       let loc = (pos1, pos2) in
                       `Seq (loc, $1) }
;

/* node in which opening and closing parentheses don't need to match */
unode:
| node          { $1 }
| OP_PAREN      { `Text ($1, false, "(") }
| CL_PAREN      { `Text ($1, false, ")") }
| COMMA         { `Text ($1, false, ",") }
;

/* node in which parentheses must be closed */
pnode:
| node          { $1 }
| OP_PAREN pnode_or_comma_list0 CL_PAREN
                { let nodes =
                    `Text ($1, false, "(") ::
                    $2 @
                    `Text ($3, false, ")") ::
                    []
                  in
                  let pos1, _ = $1
                  and _, pos2 = $3 in
                  let loc = (pos1, pos2) in
                  `Seq (loc, nodes) }
;

/* node without parentheses handling (need to use unode or pnode) */
node:
| TEXT          { `Text $1 }

| IDENT         { let loc, name = $1 in
                  `Ident (loc, name, []) }

| FUNIDENT actuals1 CL_PAREN
                {
                (* macro application that receives at least one argument,
                   possibly empty.  We cannot distinguish syntactically between
                   zero argument and one empty argument.
                *)
                  let (pos1, _), name = $1 in
                  let _, pos2 = $3 in
                  assert ($2 <> []);
                  `Ident ((pos1, pos2), name, $2) }
| FUNIDENT error
                { error (fst $1) "Invalid macro application" }

| CURRENT_LINE  { `Current_line $1 }
| CURRENT_FILE  { `Current_file $1 }

| DEF body ENDEF
                { let (pos1, _), name, formals = $1 in
                  let loc, body = $2 in
                  (* Additional spacing is needed for cases like 'foo()bar'
                     where 'foo()' expands into 'abc', giving 'abcbar'
                     instead of 'abc bar';
                     Also needed for '+foo()+' expanding into '++' instead
                     of '+ +'. *)
                  let safe_space = `Text ($3, true, " ") in
                  let body = body @ [safe_space] in
                  let body = `Seq (loc, body) in
                  let _, pos2 = $3 in
                  `Def ((pos1, pos2), name, formals, body) }

| DEF body EOF
                { let loc, _name, _formals = $1 in
                  error loc "This #def is never closed: perhaps #enddef is missing" }
                /* We include this rule in order to produce a good error message
                   when a #def has no matching #enddef. */

| SCOPE body ENDSCOPE
                { let body = `Seq $2 in
                  `Scope body }

| SCOPE body EOF
                { let loc = $1 in
                  error loc "This #scope is never closed: perhaps #endscope is missing" }
                /* We include this rule in order to produce a good error message
                   when a #scope has no matching #endscope. */

| UNDEF
                { `Undef $1 }
| WARNING
                { `Warning $1 }
| ERROR
                { `Error $1 }

| INCLUDE
                { `Include $1 }

| EXT
                { `Ext $1 }

| IF test unode_list0 elif_list ENDIF
                { let pos1, _ = $1 in
                  let _, pos2 = $5 in
                  let loc = (pos1, pos2) in
                  let test = $2 in
                  let if_true = $3 in
                  let if_false =
                    List.fold_right (
                      fun (loc, test, if_true) if_false ->
                        [`Cond (loc, test, if_true, if_false) ]
                    ) $4 []
                  in
                  `Cond (loc, test, if_true, if_false)
                }

| IF test unode_list0 elif_list error
                { (* BUG? ocamlyacc fails to reduce that rule but not menhir *)
                  error $1 "missing #endif" }

| IFDEF unode_list0 elif_list ENDIF
                { let (pos1, _), test = $1 in
                  let _, pos2 = $4 in
                  let loc = (pos1, pos2) in
                  let if_true = $2 in
                  let if_false =
                    List.fold_right (
                      fun (loc, test, if_true) if_false ->
                        [`Cond (loc, test, if_true, if_false) ]
                    ) $3 []
                  in
                  `Cond (loc, test, if_true, if_false)
                }

| IFDEF unode_list0 elif_list error
                { error (fst $1) "missing #endif" }

| LINE          { `Line $1 }
;


elif_list:
  ELIF test unode_list0 elif_list
                   { let pos1, _ = $1 in
                     let pos2 = Parsing.rhs_end_pos 4 in
                     ((pos1, pos2), $2, $3) :: $4 }
| ELSE unode_list0
                   { let pos1, _ = $1 in
                     let pos2 = Parsing.rhs_end_pos 2 in
                     [ ((pos1, pos2), `True, $2) ] }
|                  { [] }
;

actuals1:
  actual COMMA actuals1 { $1 :: $3  }
| actual                { [ $1 ] }
;

pnode_or_comma_list0:
| pnode pnode_or_comma_list0   { $1 :: $2 }
| COMMA pnode_or_comma_list0   { `Text ($1, false, ",") :: $2 }
|                              { [] }
;

test:
  bexpr ENDTEST { $1 }
;

/* Boolean expressions after #if or #elif */
bexpr:
  | TRUE                            { `True }
  | FALSE                           { `False }
  | DEFINED IDENT                   { `Defined (snd $2) }
  | OP_PAREN bexpr CL_PAREN         { $2 }
  | NOT bexpr                       { `Not $2 }
  | bexpr AND bexpr                 { `And ($1, $3) }
  | bexpr OR bexpr                  { `Or ($1, $3) }
  | aexpr EQ aexpr                  { `Eq ($1, $3) }
  | aexpr LT aexpr                  { `Lt ($1, $3) }
  | aexpr GT aexpr                  { `Gt ($1, $3) }
  | aexpr NE aexpr                  { `Not (`Eq ($1, $3)) }
  | aexpr LE aexpr                  { `Not (`Gt ($1, $3)) }
  | aexpr GE aexpr                  { `Not (`Lt ($1, $3)) }
;

/* Arithmetic expressions within boolean expressions */
aexpr:
  | INT                      { `Int $1 }
  | IDENT                    { `Ident $1 }
  | OP_PAREN aexpr_list CL_PAREN
                             { match $2 with
                               | [x] -> x
                               | l ->
                                 let pos1, _ = $1 in
                                 let _, pos2 = $3 in
                                 `Tuple ((pos1, pos2), l)
                             }
  | aexpr PLUS aexpr         { `Add ($1, $3) }
  | aexpr MINUS aexpr        { `Sub ($1, $3) }
  | aexpr STAR aexpr         { `Mul ($1, $3) }
  | aexpr SLASH aexpr        { `Div ($2, $1, $3) }
  | aexpr MOD aexpr          { `Mod ($2, $1, $3) }
  | aexpr LSL aexpr          { `Lsl ($1, $3) }
  | aexpr LSR aexpr          { `Lsr ($1, $3) }
  | aexpr ASR aexpr          { `Asr ($1, $3) }
  | aexpr LAND aexpr         { `Land ($1, $3) }
  | aexpr LOR aexpr          { `Lor ($1, $3) }
  | aexpr LXOR aexpr         { `Lxor ($1, $3) }
  | LNOT aexpr               { `Lnot $2 }
  | MINUS aexpr %prec UMINUS { `Neg $2 }
;

aexpr_list:
  | aexpr COMMA aexpr_list   { $1 :: $3 }
  | aexpr                    { [$1] }
;
