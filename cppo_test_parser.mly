%{
  open Printf
  open Cppo_types

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

%token TRUE FALSE DEFINED OP_PAREN CL_PAREN NOT AND OR EQ LT GT NE LE GE
       PLUS MINUS STAR SLASH MOD LNOT LSL LSR ASR LAND LOR LXOR EOF

%token < int64 > INT
%token < string > IDENT

%left OR
%left AND

%left PLUS MINUS
%left STAR SLASH
%left MOD LSL LSR ASR LAND LOR LXOR
%nonassoc NOT
%nonassoc LNOT
%nonassoc UMINUS

%start main
%type < Cppo_types.bool_expr > main
%%

main:
  expr EOF  { $1 }
;

expr:
  | TRUE                            { `True }
  | FALSE                           { `False }
  | DEFINED OP_PAREN IDENT CL_PAREN { `Defined $3 }
  | OP_PAREN expr CL_PAREN          { $2 }
  | NOT expr                        { `Not $2 }
  | expr AND expr                   { `And ($1, $3) }
  | expr OR expr                    { `Or ($1, $3) }
  | aexpr EQ aexpr                  { `Eq ($1, $3) }
  | aexpr LT aexpr                  { `Lt ($1, $3) }
  | aexpr GT aexpr                  { `Gt ($1, $3) }
  | aexpr NE aexpr                  { `Not (`Eq ($1, $3)) }
  | aexpr LE aexpr                  { `Not (`Gt ($1, $3)) }
  | aexpr GE aexpr                  { `Not (`Lt ($1, $3)) }
;

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
