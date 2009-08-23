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
%token ENDEF ELSE ENDIF
%token < Cppo_types.bool_expr > IF ELIF

/* Regular program */
%token OP_PAREN CL_PAREN COMMA
%token < string > TEXT IDENT

%token EOF

%start main
%type < Cppo_types.ast list > main
%%

main:
  ast main { $1 :: $2 }
| EOF      { [] }
;

ast_list0:
  ast ast_list0  { $1 :: $2 }
|                { [] }
;

/* TODO: make lone COMMAs valid only in "main" rule */
/* TODO: same for parentheses */
ast:
  TEXT          { `Text (rhs_loc 1, $1) }

| IDENT OP_PAREN args1 CL_PAREN
                /* macro application that receives at least one argument,
                   possibly empty.  We cannot distinguish syntactically between
		   zero argument and one empty argument.
                */
                { `Ident (rhs_loc2 1 4, $1, Some $3) }

| IDENT         { `Ident (rhs_loc 1, $1, None) }

| DEF ast_list0 ENDEF
                { let name = $1 in
		  let body = $2 in
		  `Def (rhs_loc2 1 3, name, body) }

| DEFUN def_args1 CL_PAREN ast_list0 ENDEF
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
                { `Include (rhs_loc 1, lazy (!Cppo_types.parse_file $1)) }


| IF ast_list0 elif_list ENDIF
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

| IF ast_list0 ELSE ast_list0 ENDIF
                { `Cond (rhs_loc2 1 5, $1, $2, $4) }
;

elif_list:
  ELIF ast_list0 elif_list
                   { (rhs_loc2 1 3, $1, $2) :: $3 }
|                  { [] }
;


args1:
  ast_list0 COMMA args1   { $1 :: $3  }
| ast_list0               { [ $1 ] }
;

def_args1:
  IDENT COMMA def_args1
                   { $1 :: $3 }
| IDENT            { [ $1 ] }
;
