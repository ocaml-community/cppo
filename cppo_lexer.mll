(* $Id$ *)
{
  
open Printf
open Lexing

open Cppo_types

let loc lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let lexer_error lexbuf descr =
  error (loc lexbuf) descr

let eval_bool_expr e x =
  let rec eval e = function
      `Defined s -> String_map.mem s e.defs
    | `Not x -> not (eval e x)
    | `And (x, y) -> eval e x && eval e y
    | `Or (x, y) -> eval e x || eval e y
  in
  eval e x


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
  mutable line_start : bool;
  mutable in_directive : bool;
  buf : Buffer.t
}

let new_line e lb =
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


rule line e = parse
    blank* "#" as s
        { 
	  if e.line_start then (
	    e.in_directive <- true;
	    directive e s lexbuf
	  )
	  else (
	    e.in_directive <- false;
	    clear e;
	    add e s
	    token e lexbuf
	  )
	}

  | ""  { e.in_directive <- false;
	  clear e;
	  token e lexbuf }

and directive e linestart = parse
    blank* "define" blank+ (ident as id) "(" 
      { DEFUN id }

  | blank* "define" blank+ (ident as id)
      { DEF id }

  | blank* "undef" blank* (ident as id) blank* eof
      { UNDEF id }

  | blank* "if"    { IF (parse_test lexbuf) }
  | blank* "elif"  { ELSIF (parse_test lexbuf) }
  | blank* "ifdef" blank* (ident as id) blank* eof
      { IF (`Defined id) }

  | blank* "ifndef" blank* (ident as id) blank* eof
      { IF (`Not (`Defined id)) }

  | blank* "else" blank* eof   { ELSE }
  | blank* "endif" blank* eof  { ENDIF }

  | blank* "include"
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	INCLUDE (get e) }
  
  | blank* "error"
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	ERROR (get e) }

  | blank* "warning"
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	WARNING (get e) }

  | blank* (['0'-'9']+ as lnum) blank* eof
      { LINE (None, int_of_string lnum) }

  | blank* (['0'-'9']+ as lnum) blank* '"'
      { clear e;
	eval_string e lexbuf;
	blank_until_eol e lexbuf;
	LINE (Some (get e), int_of_string lnum) }

  | ""  { e.is_directive <- false;
	  TEXT linestart }


and token e = parse
    "__LINE__"
      { CURRENT_LINE }

  | "__FILE__"
      { CURRENT_FILE }

  | uident
  | lident
      { TEXT (lexeme lexbuf) }

  | '`'
  | "!=" | "#" | "&" | "&&" | "(" | ")" | "*" | "+" | "," | "-"
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
	new_line lexbuf;
	if e.in_directive then
	  token e lexbuf
	else
	  TEXT (lexeme lexbuf)
      }

  | '\r'? '\n' 
      {
	new_line lexbuf;
	if e.in_directive then
	  ENDEF
	else
	  TEXT (lexeme lexbuf)
      }

  | "(*"
      { clear e;
	add e "(*";
	comment e lexbuf }

  | '"'
      { clear e;
	add "\"";
	string e lexbuf }

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
      { }


and comment e depth = parse
    "(*"
      { add e "(*";
	comment e (depth + 1) lexbuf }
      
  | "*)"
      { let depth = depth - 1 in 
	if depth > 0 then (
	  add e "*)";
	  comment e depth lexbuf
	)
      }
  | '"'
      { add e '"';
	string e lexbuf;
	comment e depth lexbuf }
      
  | '\\' '\r'? '\n'
      {
	if e.in_directive then (
	  new_line lexbuf;
	  comment e depth lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
	  comment e depth lexbuf
	)
      }

  | '\r'? '\n'
      { 
	if e.in_directive then
	  lexer_error lexbuf "Unterminated comment"
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
	  comment e depth lexbuf
	)
      }
      
  | [^'(' '*' '"' '\r' '\n']+
      { add e (lexeme lexbuf);
	comment e depth lexbuf }
      
  | eof
      { }
      
      
and string e = parse
    '"'
      { add e '"' }
      
  | "\\\\"
  | '\\' '"'
      { add e (lexeme lexbuf);
	string e lexbuf }
      
  | '\\' '\r'? '\n'
      {
	if e.in_directive then (
	  new_line lexbuf;
	  string e lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
	  string e lexbuf
	)
      }
      
  | '\r'? '\n'
      {
	if e.in_directive then
	  lexer_error lexbuf "Unterminated string literal"
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
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
	  new_line lexbuf;
	  quotation e lexbuf
	)
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
	  quotation e lexbuf
	)
      }
      
  | '\r'? '\n'
      {
	if e.in_directive then
	  lexer_error lexbuf "Unterminated quotation"
	else (
	  add e (lexeme lexbuf);
	  new_line lexbuf;
	  quotation e lexbuf
	)
      }
      
  | [^'>' '\\' '\r' '\n']+
      { add e (lexeme lexbuf);
	quotation e lexbuf }
      
  | eof 
      { lexer_error lexbuf "Unterminated quotation" }

and blank_until_eol = parse
    blank* eof        { }
  | blank* '\r'? '\n' { new_line lexbuf }
  | ""                { lexer_error lexbuf "" }

{
  let init file lexbuf =
    new_file lexbuf file;
    {
      line_start = true;
      in_directive = false;
      buf = Buffer.create 200
    }
}
