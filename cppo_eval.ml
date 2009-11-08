(* $Id$ *)

open Printf

open Cppo_types

(*
  "Tomorrow is the first day of the rest of your life",
  or the future does not affect the past.

  Unlike cpp, cppo is closure-based like OCaml.
  
  It has the following consequences:

  - macros must be defined before being referred to
  - argument-less macros are true constants
  - name shadowing is sound and legal
*)


module S = Set.Make (String)
module M = Map.Make (String)

let line_directive buf pos =
  bprintf buf "\n# %i %S\n"
    pos.Lexing.pos_lnum
    pos.Lexing.pos_fname;
  bprintf buf "%s" (String.make (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ' ')

let rec add_sep sep last = function
    [] -> [ last ]
  | [x] -> [ x; last ]
  | x :: l -> x :: sep :: add_sep sep last l 


let strip s =
  let len = String.length s in
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let first = 
    let x = ref len in
    (try
       for i = 0 to len - 1 do
	 if not (is_space s.[i]) then (
	   x := i;
	   raise Exit
	 )
       done
     with Exit -> ()
    );
    !x
  in
  let last =
    let x = ref (-1) in
    (try
       for i = len - 1 downto 0 do
	 if not (is_space s.[i]) then (
	   x := i;
	   raise Exit
	 )
       done
     with Exit -> ()
    );
    !x
  in
  if first <= last then
    String.sub s first (last - first + 1)
  else
    ""

let int_of_string_with_space s =
  try Some (Int64.of_string (strip s))
  with _ -> None

let rec eval_int env (x : arith_expr) =
  match x with
      `Int x -> x
    | `Ident (loc, name) ->
	let l =
	  try M.find name env
	  with Not_found -> error loc (sprintf "Undefined identifier %S" name)
	in
	let text =
	  List.map (
	    function
		`Text s -> s
	      | _ ->
		  error loc
		    (sprintf "Identifier %S is not bound to a constant" name)
	  ) l
	in
	let s = String.concat "" text in
	(match int_of_string_with_space s with
	     None -> 
	       error loc 
		 (sprintf "Identifier %S is not bound to an int literal" name)
	   | Some n -> n
	)

    | `Neg x -> Int64.neg (eval_int env x)
    | `Add (a, b) -> Int64.add (eval_int env a) (eval_int env b)
    | `Sub (a, b) -> Int64.sub (eval_int env a) (eval_int env b)
    | `Mul (a, b) -> Int64.mul (eval_int env a) (eval_int env b)
    | `Div (loc, a, b) ->
	(try Int64.div (eval_int env a) (eval_int env b)
	 with Division_by_zero ->
	   error loc "Division by zero")

    | `Mod (loc, a, b) -> 
	(try Int64.rem (eval_int env a) (eval_int env b)
	 with Division_by_zero ->
	   error loc "Division by zero")

    | `Lnot a -> Int64.lognot (eval_int env a)

    | `Lsl (a, b) ->
	let n = eval_int env a in
	let shift = eval_int env b in
	if shift >= 64L || shift <= 64L then 0L
	else 
	  Int64.shift_left n (Int64.to_int shift)

    | `Lsr (a, b) ->
	let n = eval_int env a in
	let shift = eval_int env b in
	if shift >= 64L || shift <= 64L then 0L
	else 
	  Int64.shift_right_logical n (Int64.to_int shift)

    | `Asr (a, b) ->
	let n = eval_int env a in
	let shift = eval_int env b in
	if shift >= 64L || shift <= 64L then 0L
	else 
	  Int64.shift_right n (Int64.to_int shift)

    | `Land (a, b) -> Int64.logand (eval_int env a) (eval_int env b)
    | `Lor (a, b) -> Int64.logor (eval_int env a) (eval_int env b)
    | `Lxor (a, b) -> Int64.logxor (eval_int env a) (eval_int env b)
	

let rec eval_bool env (x : bool_expr) =
  match x with
      `True -> true
    | `False -> false
    | `Defined s -> M.mem s env
    | `Not x -> not (eval_bool env x)
    | `And (a, b) -> eval_bool env a && eval_bool env b
    | `Or (a, b) -> eval_bool env a || eval_bool env b
    | `Eq (a, b) -> eval_int env a = eval_int env b
    | `Lt (a, b) -> eval_int env a < eval_int env b
    | `Gt (a, b) -> eval_int env a > eval_int env b

    


let rec parse file ic =
  let lexbuf = Lexing.from_channel ic in
  let lexer_env = Cppo_lexer.init file lexbuf in
  Cppo_parser.main (Cppo_lexer.line lexer_env) lexbuf

and include_file buf included file env =
  if S.mem file included then
    failwith (sprintf "Cyclic inclusion of file %S" file)
  else
    let ic = open_in_bin file in
    let l = parse ic in
    close_in ic;
    expand_list buf (S.add file included) env l

and expand_list buf included env l =
  List.fold_left (expand_node buf included) env l

and expand_node buf included env x =
  match x with
      `Ident (loc, name, opt_args) ->
	let def =
	  try Some (M.find name env)
	  with Not_found -> None
	in
	(match def, opt_args with
	     None, None -> expand_node buf included env (`Text name)
	   | None, Some args ->
	       let with_sep = 
		 add_sep [[`Text ","]] [[`Text ")"]] args in
	       let l = `Text (name ^ "(") :: List.flatten with_sep in
	       expand_list buf included env l
		 
	   | Some (`Defun (_, _, arg_names, _)), None ->
	       error loc 
		 (sprintf "%S expects %i arguments but is applied to none." 
		    name (List.length arg_names))
		 
	   | Some (`Def _), Some l ->
	       error loc 
		 (sprintf "%S expects no arguments" name)
		 
	   | Some (`Def (_, _, l)), None ->
	       (* should we expand the body? *)
	       expand_list buf included env l
		 
	   | Some (`Defun (loc, _, arg_names, l)), Some args ->
	       (* should we restore the original environment? *)
	       let argc = List.length arg_names in
	       let n = List.length args in
	       let args =
		 (* it's ok to pass an empty arg if one arg
		    is expected *)
		 if n = 0 && argc = 1 then [[]]
		 else args
	       in
	       if argc <> n then
		 error loc
		   (sprintf "%S expects %i arguments but is applied to \
                               %i arguments."
		      name argc n)
	       else
		 let app_env =
		   List.fold_left2 (
		     fun env name l -> M.add name (`Def (loc, name, l)) env
		   ) env arg_names args
		 in
		 ignore (expand_list buf included app_env l);
		 env
	)

    | `Def (loc, name, body) as x -> 
	M.add name x env

    | `Defun (loc, name, arg_names, body) as x ->
	M.add name x env

    | `Undef (loc, name) ->
	M.remove name env

    | `Include (loc, file) ->
	include_file buf included file env

    | `Cond (loc, test, if_true, if_false) ->
	let l =
	  if eval_bool env test then if_true
	  else if_false
	in
	expand_list buf included l

    | `Error (loc, msg) ->
	error loc msg

    | `Warning (loc, msg) ->
	warning loc msg;
	env

    | `Text (loc, s) ->
	Buffer.add_string buf s;
	env

    | `Seq (loc, l) ->
	expand_list buf included l


let include_channels buf env l =
  List.fold_left (
    fun env (file, open_, close) ->
      let l = Cppo_eval.parse file (open_ ()) in
      close ();
      expand_list buf (S.add file S.empty) env l
  ) env l


