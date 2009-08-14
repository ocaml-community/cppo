(* $Id$ *)
{
  
open Printf
open Lexing

open Cppo_types

type state =
    [ `Normal
    | `If of bool (* true/false 'if' block = local success *)
    | `Else of bool (* success so far *)
    | `Elif of (bool * bool) (* (success so far, local success) *) ]

type bool_expr =
    [ `Defined of string
    | `Not of bool_expr
    | `And of (bool_expr * bool_expr)
    | `Or of (bool_expr * bool_expr) ]

type env = {
  mutable add : string -> unit;
  mutable really_add : string -> unit;
  mutable in_macro : bool;
  mutable ignore : bool;
  stack : state Stack.t; (* used for conditionals *)
  mutable defs : macro_defs;
  parent_files : string Stack.t; (* to prevent recursive inclusion *)
}



let loc lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)


let add_line_directive e pos =
  if not e.ignore then
    e.really_add (make_line_directive pos)

let lexer_error lexbuf descr =
  error (loc lexbuf) descr


let define e name value =
  e.defs <- String_map.add name value e.defs

let undef e name =
  e.defs <- String_map.remove name e.defs


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
  Stack.push x e.stack;
  set_ignore e

let pop e = 
  ignore (Stack.pop e.stack);
  set_ignore e

let replace e x = 
  ignore (Stack.pop e.stack);
  push e x


let eval_bool_expr e x =
  let rec eval e = function
      `Defined s -> String_map.mem s e.defs
    | `Not x -> not (eval e x)
    | `And (x, y) -> eval e x && eval e y
    | `Or (x, y) -> eval e x || eval e y
  in
  eval e x


let enter_if lb e cond1 =
  assert (not e.in_macro);
  let cond =
    match top e with
	`Normal -> cond1
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
    | `Elif _
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
	pos_lnum = p.pos_lnum + !n;
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

let stack_mem st x =
  try
    Stack.iter (fun y -> if x = y then raise Exit) st;
    true
  with Exit -> false

let include_file loc lexer e file =
  if stack_mem e.parent_files file then
    error loc (sprintf "Recursive inclusion of file %S" file)
  else (
    Stack.push file e.parent_files;
    let ic = open_in file in
    let lb = Lexing.from_channel ic in
    new_file lb file;
    add_line_directive e lb.lex_curr_p;
    lexer e lb;
    close_in ic;
    ignore (Stack.pop e.parent_files)
  )
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
    blank* "#" as w
      {
	let initial_loc = loc lexbuf in
	let (initial_pos1, initial_pos2) = initial_loc in
	let buf1 = Buffer.create 100 in (* original text *)
	let buf2 = Buffer.create 100 in (* "\\\n" removed *)
	let added_breaks = cont_line buf1 buf2 0 lexbuf in
	let added_chars = Buffer.length buf1 in
	let single_line = Buffer.contents buf2 in
	let local_lexbuf = Lexing.from_string single_line in
	let d = directive local_lexbuf in
	update_pos lexbuf initial_pos2 added_chars added_breaks;
	let final_pos = lexbuf.lex_curr_p in
	let full_loc = (initial_pos1, final_pos) in
	match d with
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
	      enter_if lexbuf e cond
	  | `Elif expr ->
	      let cond = eval_bool_expr e expr in
	      enter_elif lexbuf e cond
	  | `Else ->
	      enter_else lexbuf e
	  | `Endif ->
	      enter_endif lexbuf e
	  | `Include file ->
	      if not e.ignore then
		include_file full_loc line e file
	  | `Error s ->
	      if not e.ignore then 
		error full_loc s
	  | `Warning s ->
	      if not e.ignore then
		warning full_loc s
	  | `Line (to_clear, opt_file, n) ->
	      set_lnum lexbuf opt_file n;
	      e.add w;
	      e.add (Buffer.contents buf1)
      }

  | ""  { token e lexbuf }


and cont_line buf1 buf2 n = parse
    [^'\n']+ as s
      { Buffer.add_string buf1 s;
	Buffer.add_string buf2 s;
	cont_line buf1 buf2 n lexbuf }
  | "\\" ('\r'? '\n' as s2) as s1
      { Buffer.add_string buf1 s1;
	Buffer.add_string buf2 s2;
	cont_line buf1 buf2 (n + 1) lexbuf }
  | '\n'
      { n + 1 }


and directive = parse
    blank* "define" blank+ (ident as id) "(" 
      { let l1 = macro_def_args lexbuf in (* TODO *)
	let l2 = macro_def_value lexbuf in (* TODO *)
	`Define (id, Some l1, l2) }

  | blank* "define" blank+ (ident as id)
      { let l2 = macro_def_value lexbuf in (* TODO *)
	`Define (id, None, l2) }

  | blank* "undef" blank* (ident as id) blank* eof
      { `Undef id }

  | blank* "if"    { failwith "not implemented" (* TODO *) }
  | blank* "elif"  { failwith "not implemented" (* TODO *) }
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

  | ""  { `Unknown }


and token e = parse
    "__LINE__"
      { e.add (string_of_int (current_line lexbuf));
	add_line_directive e (snd (loc lexbuf));
	token e lexbuf }

  | "__FILE__"
      { e.add (sprintf "%S" (current_file lexbuf));
	add_line_directive e (snd (loc lexbuf));
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
    { e.add (lexeme lexbuf);
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
      
  | '\\' 'x' (hex as c1) (hex as c2)
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
  | ""         { failwith "syntax error" }

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
	stack = st;
      }
      in
      e.add <- (fun s -> if not e.ignore then e.really_add s);
      e
    in

    List.iter (
      fun (fname, op, close) ->
	let lb = Lexing.from_channel (op ()) in
	try 
	  new_file lb e fname;
	  add_line_directive e lb.lex_curr_p;
	  line env lb;
	  close ()
	with ex ->
	  flush stdout;
	  close ();
	  raise ex
    ) in_channels
}
