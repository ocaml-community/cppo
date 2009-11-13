/* $Id$ */
%{
  open Printf
  open Cppo_types

  let print = print_string

  let rhs_loc n = (Parsing.rhs_start_pos n, Parsing.rhs_end_pos n)
  let rhs_loc2 n1 n2 = (Parsing.rhs_start_pos n1, Parsing.rhs_end_pos n2)
                    
  let unclosed opening_name opening_num closing_name closing_num =
    let msg = 
      sprintf "%s:\nSyntax error: '%s' expected.\n\
               %s:\nThis '%s' might be unmatched."
        (string_of_loc (rhs_loc closing_num)) closing_name
        (string_of_loc (rhs_loc opening_num)) opening_name in
    failwith msg

  let syntax_error s num =
    error (rhs_loc num) s
%}

/* Directives */
%token < string > DEF DEFUN UNDEF INCLUDE WARNING ERROR
%token < (string option * int) > LINE
%token < Cppo_types.bool_expr > IFDEF
%token ENDEF IF ELIF ELSE ENDIF ENDTEST


/* Boolean expressions in #if/#elif directives */
%token OP_PAREN TRUE FALSE DEFINED NOT AND OR EQ LT GT NE LE GE
       PLUS MINUS STAR SLASH MOD LNOT LSL LSR ASR LAND LOR LXOR
%token < int64 > INT


/* Regular program and shared terminals */
%token CL_PAREN COMMA CURRENT_LINE CURRENT_FILE
%token < string > IDENT FUNIDENT
%token < (bool * string) > TEXT /* bool is true for space tokens */
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
  CL_PAREN   { `Text (rhs_loc 1, (false, ")")) }
| COMMA      { `Text (rhs_loc 1, (false, ",")) }
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
  TEXT          { `Text (rhs_loc 1, $1) }

| IDENT         { `Ident (rhs_loc 1, $1, None) }

| FUNIDENT args1 CL_PAREN
                {
                (* macro application that receives at least one argument,
                   possibly empty.  We cannot distinguish syntactically between
		   zero argument and one empty argument.
                *)
		  `Ident (rhs_loc2 1 3, $1, Some $2) }
| FUNIDENT error
                { syntax_error "Invalid macro application" 1 }

| CURRENT_LINE  { `Current_line (rhs_loc 1) }
| CURRENT_FILE  { `Current_file (rhs_loc 1) }

| DEF full_node_list0 ENDEF
                { let name = $1 in
		  let body = $2 in
		  `Def (rhs_loc2 1 3, name, body) }

| DEFUN def_args1 CL_PAREN full_node_list0 ENDEF
                { let name = $1 in
		  let args = $2 in
		  let body = $4 in
		  `Defun (rhs_loc2 1 4, name, args, body) }

| UNDEF
                { `Undef (rhs_loc 1, $1) }
| WARNING
                { `Warning (rhs_loc 1, $1) }
| ERROR
                { `Error (rhs_loc 1, $1) }

| INCLUDE
                { `Include (rhs_loc 1, $1) }


| IF test full_node_list0 elif_list ENDIF
                { let loc = rhs_loc2 1 5 in
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
		  syntax_error "missing #endif" 1 }

| IFDEF full_node_list0 elif_list ENDIF
                { let loc = rhs_loc2 1 4 in
		  let test = $1 in
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
                { syntax_error "missing #endif" 1 }

| IF test full_node_list0 ELSE full_node_list0 ENDIF
                { `Cond (rhs_loc2 1 5, $2, $3, $5) }

| IF test full_node_list0 ELSE full_node_list0 error
                { syntax_error "missing #endif" 1 }

| IFDEF full_node_list0 ELSE full_node_list0 ENDIF
                { `Cond (rhs_loc2 1 4, $1, $2, $4) }

| IFDEF full_node_list0 ELSE full_node_list0 error
                { syntax_error "missing #endif" 1 }

| LINE          { `Line $1 }
;


elif_list:
  ELIF test full_node_list0 elif_list
                   { (rhs_loc2 1 4, $2, $3) :: $4 }
|                  { [] }
;


args1:
  node_list0 COMMA args1   { $1 :: $3  }
| node_list0               { [ $1 ] }
;

def_args1:
  IDENT COMMA def_args1
                   { $1 :: $3 }
| IDENT            { [ $1 ] }
;

test:
  bexpr ENDTEST { $1 }
;

/* Boolean expressions after #if or #elif */
bexpr:
  | TRUE                            { `True }
  | FALSE                           { `False }
  | DEFINED OP_PAREN IDENT CL_PAREN { `Defined $3 }
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
  | IDENT                    { `Ident (rhs_loc 1, $1) }
  | OP_PAREN aexpr CL_PAREN  { $2 }
  | aexpr PLUS aexpr         { `Add ($1, $3) }
  | aexpr MINUS aexpr        { `Sub ($1, $3) }
  | aexpr STAR aexpr         { `Mul ($1, $3) }
  | aexpr SLASH aexpr        { `Div (rhs_loc2 1 3, $1, $3) }
  | aexpr MOD aexpr          { `Mod (rhs_loc2 1 3, $1, $3) }
  | aexpr LSL aexpr          { `Lsl ($1, $3) }
  | aexpr LSR aexpr          { `Lsr ($1, $3) }
  | aexpr ASR aexpr          { `Lsr ($1, $3) }
  | aexpr LAND aexpr         { `Land ($1, $3) }
  | aexpr LOR aexpr          { `Lor ($1, $3) }
  | aexpr LXOR aexpr         { `Lxor ($1, $3) }
  | LNOT aexpr               { `Lnot $2 }
  | MINUS aexpr %prec UMINUS { `Neg $2 }
;
