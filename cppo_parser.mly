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
%token < string > DEF DEFUN INCLUDE WARNING ERROR
%token ENDEF ELSE ENDIF
%token < Cppo_types.bool_expr > IF ELIF

/* Regular program */
%token OP_PAREN CL_PAREN COMMA
%token < string > TEXT IDENT

%token EOF

%start main
%type < string list > main
%%

main:
  ast main { $1 @ $2 }
| EOF     { [] }

ast:
  TEXT          { [ (rhs_loc $1, `Text $1) ] }

| IDENT OP_PAREN args CL_PAREN
                { [ `Ident (rhs_loc2 1 4, $1, Some $3) ] }

| IDENT OP_PAREN args error 
                { unclosed "(" 2 ")" 4 }

| IDENT         { [ `Ident (rhs_loc 1, $1, None) ] }

;

args:
  top COMMA args   { let arg_list, arg_text = $3 in
		     let l = $1 in
		     (l @ arg_list), (l @ snd $2 :: arg_text) }
| top              { ($1, $1) }
;
