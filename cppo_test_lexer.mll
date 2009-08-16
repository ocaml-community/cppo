(* $Id$ *)
{
open Printf
open Lexing

open Cppo_types
open Cppo_test_parser

let loc lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- 
    { p with
	pos_lnum = p.pos_lnum + n;
	pos_bol = p.pos_cnum
    }
}

let upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let lower = ['a'-'z' '\223'-'\246' '\248'-'\255']
let digit = ['0'-'9']
let identchar = upper | lower | digit | ['_' '\'']

let lident = (lower | '_' identchar) identchar*
let uident = upper identchar*

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let oct = ['0'-'7']
let bin = ['0'-'1']

let blank = [ ' ' '\t' ]

rule token = parse
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "defined" { DEFINED }
  | "("       { OP_PAREN }
  | ")"       { CL_PAREN }
  | "&&"      { AND }
  | "||"      { OR }
  | "="       { EQ }
  | "<"       { LT }
  | ">"       { GT }
  | "<>"      { NE }
  | "<="      { LE }
  | ">="      { GE }

  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
	 | ("0o"| "0O") oct (oct | '_')*	
	 | ("0b"| "0B") bin (bin | '_')* )
      { let s = Lexing.lexeme lexbuf in
	try INT (Int64.of_string s)
	with _ -> 
	  error (loc lexbuf)
	    (sprintf "Integer constant %s is out the valid range for int64" s)
      }

  | uident
  | lident
      { IDENT (Lexing.lexeme lexbuf) }

  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { STAR }
  | "/"       { SLASH }
  | "mod"     { MOD }
  | "lsl"     { LSL }
  | "lsr"     { LSR }
  | "land"    { LAND }
  | "lor"     { LOR }
  | "lxor"    { LXOR }
  | "lnot"    { LNOT }

  | blank+                   { token lexbuf }
  | '\\' '\r'? '\n'          { new_lines lexbuf 1;
			       token lexbuf }
  | ('\r'? '\n')? eof        { EOF }
  | _          { error (loc lexbuf)
		   (sprintf "Invalid token %s" (Lexing.lexeme lexbuf)) }
