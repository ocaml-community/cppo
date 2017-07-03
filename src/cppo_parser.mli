type token =
  | DEF of ( Cppo_types.loc * string )
  | DEFUN of ( Cppo_types.loc * string )
  | UNDEF of ( Cppo_types.loc * string )
  | INCLUDE of ( Cppo_types.loc * string )
  | WARNING of ( Cppo_types.loc * string )
  | ERROR of ( Cppo_types.loc * string )
  | LINE of ( Cppo_types.loc * string option * int )
  | IFDEF of ( Cppo_types.loc * Cppo_types.bool_expr )
  | EXT of ( Cppo_types.loc * string * string )
  | ENDEF of ( Cppo_types.loc )
  | IF of ( Cppo_types.loc )
  | ELIF of ( Cppo_types.loc )
  | ELSE of ( Cppo_types.loc )
  | ENDIF of ( Cppo_types.loc )
  | ENDTEST of ( Cppo_types.loc )
  | TRUE
  | FALSE
  | DEFINED
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | GT
  | NE
  | LE
  | GE
  | PLUS
  | MINUS
  | STAR
  | LNOT
  | LSL
  | LSR
  | ASR
  | LAND
  | LOR
  | LXOR
  | OP_PAREN of ( Cppo_types.loc )
  | SLASH of ( Cppo_types.loc )
  | MOD of ( Cppo_types.loc )
  | INT of ( int64 )
  | CL_PAREN of ( Cppo_types.loc )
  | COMMA of ( Cppo_types.loc )
  | CURRENT_LINE of ( Cppo_types.loc )
  | CURRENT_FILE of ( Cppo_types.loc )
  | IDENT of ( Cppo_types.loc * string )
  | FUNIDENT of ( Cppo_types.loc * string )
  | TEXT of ( Cppo_types.loc * bool * string )
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Cppo_types.node list 
