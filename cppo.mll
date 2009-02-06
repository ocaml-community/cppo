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


type token = (string * var option)

and var =
    [ `Cvar of string
    | `Fvar of (string * token list) ]

type value =
    [ `Constant of string
    | `Function of (string list * token list)

type env = {
  add : string -> unit;
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
    sprintf "%s\nSyntax error: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg;
  failwith msg

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

let rec enter_else lb e cond1 =
  assert (not e.in_macro);
  match top e with
      `Normal -> lexer_error lb "Misplaced #else"
    | `If success0
    | `Elif (success0, _) ->
	let cond = 
	  if success0 then false
	  else cond1
	in
	replace e (`Else cond)

    | `Else cond0 -> 
	(* implicit endif *)
	pop e;
	enter_else lb e cond1

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
    
let new_line lb =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- 
    { p with
	pos_lnum = p.pos_lnum + 1;
	pos_bol = p.pos_cnum
    }
    
let shift lb n =
  lb.lex_curr_p <- { p with pos_cnum = p.pos_cnum + n }

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

let blank = [ ' ' '\t' ]
let space = [ ' ' '\t' '\r' '\n' ]


rule line e = parse
    "#" blank* "define"
  | "#" blank* "undef"
  | "#" blank* "ifdef"
  | "#" blank* "ifndef"
  | "#" blank* "else"
  | "#" blank* "elif"
  | "#" blank* "endif"
  | "#" blank* "include"
  | "#" blank* "error"
  | "#" blank* "warning"
  | "#" blank* "if"

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
      {
	add = print_string;
	in_macro = false;
	ignore = false;
	stack st
      }
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
