(* $Id$ *)
{
  
open Printf
open Lexing

type state =
    [ `Normal
    | `If of bool (* true/false 'if' block = local success *)
    | `Else of bool (* success so far *)
    | `Elif of (bool * bool) (* (success so far, local success) *) ]

module Defs = Map.Make (
  struct 
    type t = string
    let compare = String.compare
  end
)

type bool_expr =
    [ `Defined of string
    | `Not of bool_expr
    | `And of (bool_expr * bool_expr)
    | `Or of (bool_expr * bool_expr) ]

type token = (string * var option)

and var =
    [ `Cvar of string
    | `Fvar of (string * token list) ]

type value =
    [ `Constant of string
    | `Function of (string list * token list)

type env = {
  mutable add : string -> unit;
  mutable really_add : string -> unit;
  mutable in_macro : bool;
  mutable ignore : bool;
  mutable stack : state Stack.t;
  mutable defs : value Defs.t
}



let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)

let error loc s =
  let msg = 
    sprintf "%s\nError: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg;
  failwith msg

let warning loc s =
  let msg = 
    sprintf "%s\Warning: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg


let loc lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)
  

let lexer_error lexbuf descr =
  error (loc lexbuf) descr


let define e name value =
  e.defs <- Defs.add name value e.defs

let undef e name =
  e.defs <- Defs.remove name e.defs


let top e =
  try Stack.top e.stack
  with Stack.Empty -> assert false

let set_ignore e =
  match top e with
      `Normal -> e.ignore <- true
    | `If cond
    | `Elif (_, cond)
    | `Else cond ->
	e.ignore <- not cond

let push e x = 
  Stack.push e.stack x;
  set_ignore e

let pop e = 
  ignore (Stack.pop e.stack);
  set_ignore e

let replace e x = 
  ignore (Stack.pop e.stack);
  push e x


let enter_if lb e cond1 =
  assert (not e.in_macro);
  let cond =
    match top e with
	`Normal -> cond
      | `If cond0 -> cond0 && cond1
      | `Else cond0 -> cond0 && cond1
      | `Elif (_, cond0) -> cond0 && cond1
  in
  push e (`If cond)

let rec enter_else lb e =
  assert (not e.in_macro);
  match top e with
      `Normal -> lexer_error lb "Misplaced #else"
    | `If success0
    | `Elif (success0, _) ->
	let cond = not success0 in
	replace e (`Else cond)

    | `Else cond0 -> 
	(* implicit endif *)
	pop e;
	enter_else lb e

let rec enter_elif lb e cond1 =
  assert (not e.in_macro);
  let cond =
    match top e with
	`Normal -> lexer_error lb "Misplaced #elif"
      | `If success0
      | `Elif (success0, _) ->
	  let cond = 
	    if success0 then false
	    else cond1
	  in
	  replace e (`Elif (success0 || cond, cond))

      | `Else cond0 ->
	  (* implicit endif *)
	  pop e;
	  enter_elif lb e cond1

let rec enter_endif lb e =
  assert (not e.in_macro);
  match top e with
      `Normal -> lexer_error lb "Misplaced #endif"
    | `If _
    | `Elif _ ->
    | `Else _ -> pop e


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
	pos_lnum = p.pos_lnum + n;
	pos_bol = p.pos_cnum
    }

let new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- 
    { p with
	pos_lnum = p.pos_lnum + n;
	pos_bol = p.pos_cnum
    }

(* must start a new line *)
let update_pos lexbuf p added_chars added_breaks =
  let cnum = p.pos_cnum + added_chars in
  lb.lex_curr_p <-
    { pos_lnum = p.pos_lnum + added_breaks;
      pos_bol = cnum;
      pos_cnum = cnum }

let set_lnum lb opt_file lnum =
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
  lb.lex_curr_p <- { p with pos_cnum = p.pos_cnum + n }

let read_hexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'A'..'F' -> Char.code c - 55
    | 'a'..'z' -> Char.code c - 87
    | _ -> invalid_arg "read_hexdigit"

let read_hex2 c1 c2 =
  Char.chr (read_hexdigit c1 * 16 + read_hexdigit c2)


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
    "#"
      {
	let initial_loc = loc lexbuf in
	let (initial_pos1, initial_pos2) = initial_loc in
	let buf1 = Buffer.create 100 in (* original text *)
	let buf2 = Buffer.create 100 in (* "\\\n" removed *)
	let added_breaks = cont_line buf1 buf2 0 lexbuf in
	let added_chars = Buffer.length buf1 in
	let single_line = Buffer.contents buf2 in
	let local_lexbuf = Lexing.from_string real_line in
	let directive = parse_directive local_lexbuf in
	update_pos lexbuf initial_pos2 added_chars added_breaks;
	let final_pos = lexbuf.lex_curr_p in
	let full_loc = (initial_pos1, final_pos) in
	match directive with
	    `Unknown ->
	      if not e.ignore then
		e.really_add (Buffer.contents buf1)
	  | `Define (name, value) ->
	      if not e.ignore then
		define e name value
	  | `Undef name -> 
	      if not e.ignore then
		undef e name
	  | `If expr ->
	      let cond = eval_bool_expr e expr in
	      enter_if lb e cond
	  | `Elif expr ->
	      let cond = eval_bool_expr e expr in
	      enter_elif lb e cond
	  | `Else ->
	      enter_else lb e
	  | `Endif ->
	      enter_endif lb e
	  | `Include file ->
	      if not e.ignore then
		include_file line e file
	  | `Error s ->
	      if not e.ignore then 
		error full_loc s
	  | `Warning s ->
	      if not e.ignore then
		warning full_loc s
	  | `Line (to_clear, opt_file, n) ->
	      set_lnum lexbuf opt_file n;
	      e.add "#";
	      e.add (Buffer.contents buf1)
      }


and directive = parse
    blank* "define" blank* (ident as id) "(" 
      { let l1 = macro_def_args lexbuf in (* TODO *)
	let l2 = macro_def_value lexbuf in (* TODO *)
	`Define (id, Some l1, l2) }

  | blank* "undef" blank* (ident as id) blank* eof
      { `Undef id }

  | blank* "if"    { failwith "not implemented" }
  | blank* "elif"  { failwith "not implemented" }
  | blank* "ifdef" blank* (ident as id) blank* eof
      { `If (`Defined id) }

  | blank* "ifndef" blank* (ident as id) blank* eof
      { `If (`Not (`Defined id)) }

  | blank* "else" blank* eof   { `Else }
  | blank* "endif" blank* eof  { `Endif }

  | blank* "include"
      { let buf = Buffer.create 50 in
	eval_string buf lexbuf;
	blank_until_eof lexbuf;
	`Include (Buffer.contents buf) }
  
  | blank* "error"
      { let buf = Buffer.create 50 in
	eval_string buf lexbuf;
	blank_until_eof lexbuf;
	`Error (Buffer.contents buf) }

  | blank* "warning"
      { let buf = Buffer.create 50 in
	eval_string buf lexbuf;
	blank_until_eof lexbuf;
	`Warning (Buffer.contents buf) }

  | blank* (['0'-'9']+ as lnum) blank* eof
      { `Line (None, int_of_string lnum) }

  | blank* (['0'-'9']+ as lnum) blank* '"'
      { let buf = Buffer.create 50 in
	eval_string buf lexbuf;
	blank_until_eof lexbuf;
	`Line (Some (Buffer.contents buf), int_of_string lnum) }

  |   { `Unknown }


and token e = parse
    "__LINE__"
      { e.add lexbuf (string_of_int (current_line lexbuf));
	e.add_line_directive lexbuf;
	token e lexbuf }

  | "__FILE__"
      { e.add lexbuf (sprintf "%S" (current_file lexbuf));
	e.add_line_directive lexbuf;
	token e lexbuf }

  | uident
  | lident
      { e.subst e (Lexing.lexeme lexbuf);
	token e lexbuf }

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

      { e.add lexbuf (Lexing.lexeme lexbuf);
	token e lexbuf }

  | blank+
      { e.add lexbuf (Lexing.lexeme lexbuf);
	token e lexbuf }

  | '\'' ('\r'? '\n' as nl)

      { if e.in_macro then (
	  shift lexbuf 1;
	  e.add lexbuf nl
	)
	else
	  lexer_error lexbuf "Out-of-macro backslash before end of line"
      }

  | '\r'? '\n' 

      { e.add lexbuf (lexeme lexbuf);
	new_line lexbuf;
	line lexbuf }

  | "(*"
      { e.add "(*";
	comment e lexbuf;
	token e lexbuf }

  | '"'
      { e.add "\"";
	string e lexbuf;
	token e lexbuf }

  | "<" (":" lident)? ("@" lident)? "<"
      { e.add (lexeme lexbuf);
	quotation e lexbuf;
	token e lexbuf }


  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
	 | ("0o"| "0O") oct (oct | '_')*	
	 | ("0b"| "0B") bin (bin | '_')* )

  | '-'? digit (digit | '_')* ('.' (digit | '_')* )? 
      (['e' 'E'] ['+' '-']? digit (digit | '_')* )? 

  | _
    { e.add (lexeme lexbuf));
      token e lexbuf }

  | eof 
      { }


and comment e depth = parse
    "(*"
      { e.add "(*";
	comment e (depth + 1) lexbuf }
      
  | "*)"
      { let depth = depth - 1 in 
	if depth > 0 then (
	  e.add "*)";
	  comment e depth lexbuf
	)
      }
  | '"'
      { e.add '"';
	string e lexbuf;
	comment e depth lexbuf }
      
  | '\r'? '\n'
      { e.add (lexeme lexbuf);
	new_line lexbuf;
	comment e depth lexbuf }
      
  | [^'(' '*' '"' '\r' '\n']+
      { e.add (lexeme lexbuf);
	comment e depth lexbuf }
      
  | eof
      { }
      
      
and string e = parse
    '"'
      { e.add '"' }
      
  | "\\\\"
  | '\\' '"'
      { e.add (lexeme lexbuf);
	string e lexbuf }
      
  | '\r'? '\n'
      { e.add (lexeme lexbuf);
	new_line lexbuf;
	string e lexbuf }
      
  | [^ '"' '\\' '\r' '\n']+
      { e.add (lexeme lexbuf);
	string e lexbuf }
      
  | eof
      { }
      

and eval_string buf = parse
    '"'
      {  }
      
  | '\\' (['\'' '"' '\\'] as c)
      { Buffer.add_char buf c;
	eval_string buf lexbuf }

  | '\\' '\r'? '\n' blank*
      { eval_string buf lexbuf }
      
  | '\\' (digit digit digit as s)
      { Buffer.add_char buf (Char.chr (string_of_int s));
	eval_string buf lexbuf }
      
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      { Buffer.add_char buf (read_hex2 c1 c2);
	eval_string buf lexbuf }
      
  | '\\' 'b'
      { Buffer.add_char buf '\b';
	eval_string buf lexbuf }
      
  | '\\' 'n'
      { Buffer.add_char buf '\n';
	eval_string buf lexbuf }
      
  | '\\' 'r'
      { Buffer.add_char buf '\r';
	eval_string buf lexbuf }
      
  | '\\' 't'
      { Buffer.add_char buf '\t';
	eval_string buf lexbuf }
      
  | [^ '"' '\\']+
      { e.add (lexeme lexbuf);
	string e lexbuf }
      
  | eof
      { lexer_error lexbuf "Unterminated string literal" }
      
      
and quotation e = parse
    ">>"
      { e.add ">>" }
      
  | "\\>>"
      { e.add "\\>>";
	quotation e lexbuf }

  | '\r'? '\n'
      { e.add (lexeme lexbuf);
	new_line lexbuf;
	quotation e lexbuf }
      
  | [^'>' '\\' '\r' '\n']+
      { e.add (lexeme lexbuf);
	quotation e lexbuf }
      
  | eof 
      { }

and blank_until_eof = parse
    blank* eof { }
  |            { failwith "syntax error" }

{
  let () =
    let files = ref [] in
    let options = [] in
    let msg = sprintf "Usage: %s [file1 [file2 ...]]" in
    let add_file s = files := s :: !files in
    Arg.parse options add_file msg;
    
    let in_channels = 
      match List.rev !files with
	  [] -> [ (fun () -> stdin), (fun () -> ()) ]
	| l -> 
	    List.map (
	      fun file -> 
		let ic = lazy (open_in file) in
		(fun () -> Lazy.force ic), (fun () -> close_in (Lazy.force ic))
	    ) l
    in

    let env =
      let st = Stack.create () in
      Stack.add st `Normal;
      let e = {
	add = print_string;
	really_add = print_string;
	in_macro = false;
	ignore = ign;
	stack st
      }
      in
      e.add <- (fun s -> if not e.ignore then e.really_add s);
      e
    in

    List.iter (
      fun (op, close) ->
	let lb = Lexing.from_channel (op ()) in
	try 
	  line env lb;
	  close ()
	with ex ->
	  flush stdout;
	  close ();
	  raise ex
    ) in_channels
}
