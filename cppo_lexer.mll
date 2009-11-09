(* $Id$ *)
{
  
open Printf
open Lexing

open Cppo_types
open Cppo_parser

let loc lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let lexer_error lexbuf descr =
  error (loc lexbuf) descr

let new_file lb name =
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = name }

let lex_new_lines lb =
  let n = ref 0 in
  let s = lb.lex_buffer in
  for i = lb.lex_start_pos to lb.lex_curr_pos do
    if s.[i] = '\n' then
      incr n
  done;
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- 
    { p with
	pos_lnum = p.pos_lnum + !n;
	pos_bol = p.pos_cnum
    }

let count_new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- 
    { p with
	pos_lnum = p.pos_lnum + n;
	pos_bol = p.pos_cnum
    }

(* must start a new line *)
let update_pos lb p added_chars added_breaks =
  let cnum = p.pos_cnum + added_chars in
  lb.lex_curr_p <-
    { pos_fname = p.pos_fname;
      pos_lnum = p.pos_lnum + added_breaks;
      pos_bol = cnum;
      pos_cnum = cnum }

let set_lnum lb opt_file lnum =
  let p = lb.lex_curr_p in
  let cnum = p.pos_cnum in
  let fname =
    match opt_file with
	None -> p.pos_fname
      | Some file -> file
  in
  lb.lex_curr_p <-
    { pos_fname = fname;
      pos_bol = cnum;
      pos_cnum = cnum;
      pos_lnum = lnum }
	
let shift lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- { p with pos_cnum = p.pos_cnum + n }

let read_hexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'A'..'F' -> Char.code c - 55
    | 'a'..'z' -> Char.code c - 87
    | _ -> invalid_arg "read_hexdigit"

let read_hex2 c1 c2 =
  Char.chr (read_hexdigit c1 * 16 + read_hexdigit c2)

type env = {
  mutable lexer : [ `Ocaml | `Test ];
  mutable line_start : bool;
  mutable in_directive : bool;
  buf : Buffer.t
}

let new_line env lb =
  env.line_start <- true;
  count_new_lines lb 1

let clear env = Buffer.clear env.buf

let add env s =
  env.line_start <- false;
  Buffer.add_string env.buf s

let add_char env c =
  env.line_start <- false;
  Buffer.add_char env.buf c

let get env = Buffer.contents env.buf
}

let upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let lower = ['a'-'z' '\223'-'\246' '\248'-'\255']
let digit = ['0'-'9']
let identchar = upper | lower | digit | ['_' '\'']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let oct = ['0'-'7']
let bin = ['0'-'1']

let operator_char = 
  [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let infix_symbol =
  ['=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' '$' '%'] operator_char*
let prefix_symbol = ['!' '?' '~'] operator_char*

let lident = (lower | '_' identchar) identchar*
let uident = upper identchar*
let ident = lident | uident

let blank = [ ' ' '\t' ]
let space = [ ' ' '\t' '\r' '\n' ]

let line = ( [^'\n'] | '\\' ('\r'? '\n') )* ('\n' | eof)

let dblank0 = (blank | '\\' '\r'? '\n')*
let dblank1 = blank (blank | '\\' '\r'? '\n')*

rule token e = parse
    ""
      {
	(*
	  We use two different lexers for boolean expressions in #if directives
	  and for regular OCaml tokens.
	*)
	match e.lexer with
	    `Ocaml -> ocaml_token e lexbuf
	  | `Test -> test_token e lexbuf
      }

and line e = parse
    blank* "#" as s
        { 
	  match e.lexer with
	      `Test -> lexer_error lexbuf "Syntax error in boolean expression"
	    | `Ocaml ->
		if e.line_start then (
		  e.in_directive <- true;
		  clear e;
		  add e s;
		  directive e s lexbuf
		)
		else (
		  e.in_directive <- false;
		  clear e;
		  add e s;
		  ocaml_token e lexbuf
		)
	}

  | ""  { clear e;
	  token e lexbuf }

and directive e linestart = parse
    blank* "define" dblank1 (ident as id) "(" 
      { DEFUN id }

  | blank* "define" dblank1 (ident as id)
      { assert e.in_directive; DEF id }

  | blank* "undef" dblank1 (ident as id) blank* eof
      { UNDEF id }

  | blank* "if" dblank1    { e.lexer <- `Test;
			     IF }
  | blank* "elif" dblank1  { e.lexer <- `Test;
			     ELIF }

  | blank* "ifdef" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
	IFDEF (`Defined id) }

  | blank* "ifndef" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
	IFDEF (`Not (`Defined id)) }

  | blank* "else"
      { blank_until_eol e lexbuf;
	ELSE }

  | blank* "endif"
      { blank_until_eol e lexbuf;
	ENDIF }

  | blank* "include" dblank0 '"'
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	INCLUDE (get e) }
  
  | blank* "error" dblank0 '"'
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	ERROR (get e) }

  | blank* "warning" dblank0 '"'
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	WARNING (get e) }

  | blank* (['0'-'9']+ as lnum) dblank0 '\r'? '\n'
      { e.in_directive <- false;
	LINE (None, int_of_string lnum) }

  | blank* (['0'-'9']+ as lnum) dblank0 '"'
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	LINE (Some (get e), int_of_string lnum) }

  | ""  { e.in_directive <- false;
	  TEXT linestart }


and blank_until_eol e = parse
    blank* eof
  | blank* '\r'? '\n' { e.line_start <- true;
			e.in_directive <- false }
  | ""                { lexer_error lexbuf "Missing line terminator" }

and ocaml_token e = parse
    "__LINE__"
      { CURRENT_LINE }

  | "__FILE__"
      { CURRENT_FILE }

  | uident
  | lident as s
      { IDENT s }

  | uident
  | lident as s "("
      { FUNIDENT s }

  | ")"       { CL_PAREN }

  | '`'
  | "!=" | "#" | "&" | "&&" | "(" |  "*" | "+" | "," | "-"
  | "-." | "->" | "." | ".. :" | "::" | ":=" | ":>" | ";" | ";;" | "<"
  | "<-" | "=" | ">" | ">]" | ">}" | "?" | "??" | "[" | "[<" | "[>" | "[|"
  | "]" | "_" | "`" | "{" | "{<" | "|" | "|]" | "}" | "~"
  | prefix_symbol 
  | infix_symbol
  | "'\n'" 
  | "'\r\n'" 
  | "'\\\n'"
  | "'\\\r\n'"
  | "'" ([^'\'''\\'] 
  | '\\' (_ | digit digit digit | 'x' hex hex)) "'"

      { TEXT (lexeme lexbuf) }

  | blank+
      { TEXT (lexeme lexbuf) }

  | '\'' '\r'? '\n'

      {
	new_line e lexbuf;
	if e.in_directive then
	  ocaml_token e lexbuf
	else
	  TEXT (lexeme lexbuf)
      }

  | '\r'? '\n'
      {
	new_line e lexbuf;
	if e.in_directive then (
	  e.in_directive <- false;
	  ENDEF
	)
	else
	  TEXT (lexeme lexbuf)
      }

  | "(*"
      { clear e;
	add e "(*";
	comment e 1 lexbuf }

  | '"'
      { clear e;
	add e "\"";
	string e lexbuf;
	TEXT (get e) }

  | "<" (":" lident)? ("@" lident)? "<"
      { clear e;
	add e (lexeme lexbuf);
	quotation e lexbuf;
	TEXT (get e) }


  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
	 | ("0o"| "0O") oct (oct | '_')*	
	 | ("0b"| "0B") bin (bin | '_')* )

  | '-'? digit (digit | '_')* ('.' (digit | '_')* )? 
      (['e' 'E'] ['+' '-']? digit (digit | '_')* )? 

  | _
      { TEXT (lexeme lexbuf) }

  | eof 
      { EOF }


and comment e depth = parse
    "(*"
      { add e "(*";
	comment e (depth + 1) lexbuf }
      
  | "*)"
      { let depth = depth - 1 in 
	add e "*)";
	if depth > 0 then
	  comment e depth lexbuf
	else
	  TEXT (get e)
      }
  | '"'
      { add_char e '"';
	string e lexbuf;
	comment e depth lexbuf }
      
  | '\\' '\r'? '\n'
      {
	if e.in_directive then (
	  new_line e lexbuf;
	  comment e depth lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  comment e depth lexbuf
	)
      }

  | '\r'? '\n'
      { 
	if e.in_directive then
	  lexer_error lexbuf 
	    "Unterminated comment or missing \\ before new line"
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  comment e depth lexbuf
	)
      }
      
  | [^'(' '*' '"' '\r' '\n']+
      { add e (lexeme lexbuf);
	comment e depth lexbuf }
      
  | eof
      { lexer_error lexbuf "Unterminated comment reaching the end of file" }
      
      
and string e = parse
    '"'
      { add_char e '"' }
      
  | "\\\\"
  | '\\' '"'
      { add e (lexeme lexbuf);
	string e lexbuf }
      
  | '\\' '\r'? '\n'
      {
	if e.in_directive then (
	  new_line e lexbuf;
	  string e lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  string e lexbuf
	)
      }
      
  | '\r'? '\n'
      {
	if e.in_directive then
	  lexer_error lexbuf "Unterminated string literal"
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  string e lexbuf
	)
      }
      
  | [^ '"' '\\' '\r' '\n']+
      { add e (lexeme lexbuf);
	string e lexbuf }
      
  | eof
      { }
      

and eval_string e = parse
    '"'
      {  }
      
  | '\\' (['\'' '\"' '\\'] as c)
      { add_char e c;
	eval_string e lexbuf }

  | '\\' '\r'? '\n'
      { assert e.in_directive;
        eval_string e lexbuf }
      
  | '\r'? '\n'
      { assert e.in_directive;
        lexer_error lexbuf "Unterminated string literal" }
      
  | '\\' (digit digit digit as s)
      { add_char e (Char.chr (int_of_string s));
	eval_string e lexbuf }
      
  | '\\' 'x' (hex as c1) (hex as c2)
      { add_char e (read_hex2 c1 c2);
	eval_string e lexbuf }
      
  | '\\' 'b'
      { add_char e '\b';
	eval_string e lexbuf }
      
  | '\\' 'n'
      { add_char e '\n';
	eval_string e lexbuf }
      
  | '\\' 'r'
      { add_char e '\r';
	eval_string e lexbuf }
      
  | '\\' 't'
      { add_char e '\t';
	eval_string e lexbuf }
      
  | [^ '\"' '\\']+
      { add e (lexeme lexbuf);
	eval_string e lexbuf }
      
  | eof
      { lexer_error lexbuf "Unterminated string literal" }
      
      
and quotation e = parse
    ">>"
      { add e ">>" }
      
  | "\\>>"
      { add e "\\>>";
	quotation e lexbuf }

  | '\\' '\r'? '\n'
      {
	if e.in_directive then (
	  new_line e lexbuf;
	  quotation e lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  quotation e lexbuf
	)
      }
      
  | '\r'? '\n'
      {
	if e.in_directive then
	  lexer_error lexbuf "Unterminated quotation"
	else (
	  add e (lexeme lexbuf);
	  new_line e lexbuf;
	  quotation e lexbuf
	)
      }
      
  | [^'>' '\\' '\r' '\n']+
      { add e (lexeme lexbuf);
	quotation e lexbuf }
      
  | eof 
      { lexer_error lexbuf "Unterminated quotation" }

and test_token e = parse
    "true"    { TRUE }
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
  | "asr"     { ASR }
  | "land"    { LAND }
  | "lor"     { LOR }
  | "lxor"    { LXOR }
  | "lnot"    { LNOT }

  | blank+                   { test_token e lexbuf }
  | '\\' '\r'? '\n'          { new_line e lexbuf;
			       test_token e lexbuf }
  | '\r'? '\n' 
  | eof        { e.lexer <- `Ocaml;
		 assert e.in_directive;
		 e.in_directive <- false;
		 ENDTEST }
  | _          { error (loc lexbuf)
		   (sprintf "Invalid token %s" (Lexing.lexeme lexbuf)) }


{
  let init file lexbuf =
    new_file lexbuf file;
    {
      lexer = `Ocaml;
      line_start = true;
      in_directive = false;
      buf = Buffer.create 200
    }
}
