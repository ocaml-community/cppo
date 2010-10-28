/* $Id$ */
%{
  open Printf
  open Cppo_types

  let print = print_string

  let rhs_loc n1 n2 = (Parsing.rhs_start_pos n1, Parsing.rhs_end_pos n2)
%}

/* Directives */
%token < Cppo_types.loc * string > DEF DEFUN UNDEF INCLUDE WARNING ERROR
%token < Cppo_types.loc * string option * int > LINE
%token < Cppo_types.loc * Cppo_types.bool_expr > IFDEF
%token < Cppo_types.loc * string * string > EXT
%token < Cppo_types.loc > ENDEF IF ELIF ELSE ENDIF ENDTEST

/* Boolean expressions in #if/#elif directives */
%token OP_PAREN TRUE FALSE DEFINED NOT AND OR EQ LT GT NE LE GE
       PLUS MINUS STAR LNOT LSL LSR ASR LAND LOR LXOR
%token < Cppo_types.loc > SLASH MOD 
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
  full_node main { $1 :: $2 }
| EOF      { [] }
;

full_node:
  CL_PAREN   { `Text ($1, false, ")") }
| COMMA      { `Text ($1, false, ",") }
| node       { $1 }
;

node_list0:
  node node_list0  { $1 :: $2 }
|                  { [] }
;

full_node_list0:
  full_node full_node_list0  { $1 :: $2 }
|                            { [] }
;

/* TODO: make lone COMMAs valid only in "main" rule */
/* TODO: same for parentheses */
node:
  TEXT          { `Text $1 }

| IDENT         { let loc, name = $1 in
		  `Ident (loc, name, None) }

| FUNIDENT args1 CL_PAREN
                {
                (* macro application that receives at least one argument,
                   possibly empty.  We cannot distinguish syntactically between
		   zero argument and one empty argument.
                *)
		  let (pos1, _), name = $1 in
		  let _, pos2 = $3 in
		  `Ident ((pos1, pos2), name, Some $2) }
| FUNIDENT error
                { error (fst $1) "Invalid macro application" }

| CURRENT_LINE  { `Current_line $1 }
| CURRENT_FILE  { `Current_file $1 }

| DEF full_node_list0 ENDEF
                { let (pos1, _), name = $1 in

		  (* Additional spacing is needed for cases like '+foo+'
		     expanding into '++' instead of '+ +'. *)
		  let safe_space = `Text ($3, true, " ") in

		  let body = $2 @ [safe_space] in
		  let _, pos2 = $3 in
		  `Def ((pos1, pos2), name, body) }

| DEFUN def_args1 CL_PAREN full_node_list0 ENDEF
                { let (pos1, _), name = $1 in
		  let args = $2 in

		  (* Additional spacing is needed for cases like 'foo()bar'
		     where 'foo()' expands into 'abc', giving 'abcbar'
		     instead of 'abc bar';
		     Also needed for '+foo()+' expanding into '++' instead
		     of '+ +'. *)
		  let safe_space = `Text ($5, true, " ") in

		  let body = $4 @ [safe_space] in
		  let _, pos2 = $5 in
		  `Defun ((pos1, pos2), name, args, body) }

| DEFUN CL_PAREN
                { error (fst (fst $1), snd $2)
		    "At least one argument is required" }

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

| IF test full_node_list0 elif_list ENDIF
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

| IF test full_node_list0 elif_list error
                { (* BUG? ocamlyacc fails to reduce that rule but not menhir *)
		  error $1 "missing #endif" }

| IFDEF full_node_list0 elif_list ENDIF
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

| IFDEF full_node_list0 elif_list error
                { error (fst $1) "missing #endif" }

| IF test full_node_list0 ELSE full_node_list0 ENDIF
                { `Cond ((fst $1, snd $6), $2, $3, $5) }

| IF test full_node_list0 ELSE full_node_list0 error
                { error $1 "missing #endif" }

| IFDEF full_node_list0 ELSE full_node_list0 ENDIF
                { `Cond ((fst (fst $1), snd $5), (snd $1), $2, $4) }

| IFDEF full_node_list0 ELSE full_node_list0 error
                { error (fst $1) "missing #endif" }

| LINE          { `Line $1 }
;


elif_list:
  ELIF test full_node_list0 elif_list
                   { let pos1, _ = $1 in
		     let pos2 = Parsing.rhs_end_pos 4 in
		     ((pos1, pos2), $2, $3) :: $4 }
|                  { [] }
;


args1:
  node_list0 COMMA args1   { $1 :: $3  }
| node_list0               { [ $1 ] }
;

def_args1:
  IDENT COMMA def_args1
                   { (snd $1) :: $3 }
| IDENT            { [ snd $1 ] }
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
  | OP_PAREN aexpr CL_PAREN  { $2 }
  | aexpr PLUS aexpr         { `Add ($1, $3) }
  | aexpr MINUS aexpr        { `Sub ($1, $3) }
  | aexpr STAR aexpr         { `Mul ($1, $3) }
  | aexpr SLASH aexpr        { `Div ($2, $1, $3) }
  | aexpr MOD aexpr          { `Mod ($2, $1, $3) }
  | aexpr LSL aexpr          { `Lsl ($1, $3) }
  | aexpr LSR aexpr          { `Lsr ($1, $3) }
  | aexpr ASR aexpr          { `Lsr ($1, $3) }
  | aexpr LAND aexpr         { `Land ($1, $3) }
  | aexpr LOR aexpr          { `Lor ($1, $3) }
  | aexpr LXOR aexpr         { `Lxor ($1, $3) }
  | LNOT aexpr               { `Lnot $2 }
  | MINUS aexpr %prec UMINUS { `Neg $2 }
;
