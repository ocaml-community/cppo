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


type env = macro_value String_map.t

and macro_value =
    [ `Constant of string
    | `Function of (env * string list * ast list) ]

     
(* Simplified form of Netchannels.out_obj_channel from ocamlnet *)
class type out_obj_channel =
object
  method output_string : string -> unit
  method flush : unit -> unit
  method close_out : unit -> unit
end

class output_channel oc : out_obj_channel =
object
  method output_string s = output_string oc s
  method flush () = flush oc
  method close_out () = close_out_noerr oc
end

class output_buffer buf : out_obj_channel =
object
  method output_string s = Buffer.add_string buf s
  method flush () = ()
  method close_out () = ()
end


let compact_re = Str.regexp "^[ \t\n\r]*\\([^ \t\n\r]*\\)[ \t\n\r]*$"
let parse_int loc s0 = 
  assert (Str.string_match compact_re s0 0);
  let s = Str.matched_group 1 s0 in
  try Int64.of_string s
  with _ -> error loc (sprintf "Not a valid int literal: %S" s0)

let rec eval_bool env (cond : bool_expr) =
  match cond with
      `True -> true
    | `False -> false
    | `Defined name -> String_map.mem name env
    | `Not cond -> not (eval_bool env cond)
    | `And (cond1, cond2) -> 
	eval_bool env cond1 && eval_bool env cond2
    | `Or (cond1, cond2) -> 
	eval_bool env cond1 || eval_bool env cond2
    | `Eq (a, b) -> Int64.compare (eval_int env a) (eval_int env b) = 0
    | `Lt (a, b) -> Int64.compare (eval_int env a) (eval_int env b) < 0
    | `Gt (a, b) -> Int64.compare (eval_int env a) (eval_int env b) > 0

and eval_int env (x : arith_expr) =
  match x with
      `Int a -> a
    | `Ident (loc, name) -> 
	let x = 
	  try String_map.find name env
	  with Not_found -> error loc (sprintf "Undefined constant %s" name)
	in
	(match x with
	     `Constant s -> parse_int loc s
	   | `Function _ -> 
	       error loc (sprintf "%s is not a constant" name)
	)

    | `Neg a -> Int64.neg (eval_int env a)
    | `Add (a, b) -> Int64.add (eval_int env a) (eval_int env b)
    | `Sub (a, b) -> Int64.sub (eval_int env a) (eval_int env b)
    | `Mul (a, b) -> Int64.mul (eval_int env a) (eval_int env b)
    | `Div (loc, a, b) ->
	let a = eval_int env a in
	let b = eval_int env b in
	if b = 0L then
	  error loc "Division by zero"
	else
	  Int64.div a b

    | `Mod (loc, a, b) -> 
	let a = eval_int env a in
	let b = eval_int env b in
	if b = 0L then
	  error loc "Division by zero"
	else
	  Int64.rem a b

    | `Lnot a -> Int64.lognot (eval_int env a)
    | `Lsl (a, b) -> 
	Int64.shift_left
	  (eval_int env a) (Int64.to_int (eval_int env b))
    | `Lsr (a, b) -> 
	Int64.shift_right_logical 
	  (eval_int env a) (Int64.to_int (eval_int env b))
    | `Land (a, b) -> Int64.logand (eval_int env a) (eval_int env b)
    | `Lor (a, b) -> Int64.logor (eval_int env a) (eval_int env b)
    | `Lxor (a, b) -> Int64.logxor (eval_int env a) (eval_int env b)

and subst_app (env : env) loc name args =
  try
    match String_map.find name env with
	`Constant _ -> 
	  (* this is not an error for cpp, but really error-prone. *)
	  error loc
	    (sprintf "Macro %S is applied to %i arguments \
                      but it is a constant-like macro.\n\
                      Place it within parentheses."
	       name (List.length args))
      | `Function (closure_env, arg_names, body) ->
	  let arg_n = List.length arg_names in
	  let arg_c = List.length args in
	  if arg_n < arg_c then
	    error loc 
	      (sprintf "Macro %S is applied to %i arguments \
                        but expects only %i arguments."
		 name arg_c arg_n)
	  else if arg_c < arg_n then
	    error loc 
	      (sprintf "Macro %S expects %i arguments \
                        but is only applied to %i arguments."
		 name arg_n arg_c)
	  else
	    (* unlike cpp we restore the environment from the closure. *)
	    let env =
	      List.fold_left2 
		(fun env k s -> String_map.add k (`Constant s) env) 
		closure_env arg_names args
	    in
	    Some (to_string env body)
  with
      Not_found -> None


and subst_const (env : env) loc name =
  try
    match String_map.find name env with
	`Constant s -> Some s
      | `Function (_, arg_names, _) ->
	  (* this is not an error for cpp, but really error-prone. *)
	  error loc
	    (sprintf "Macro %S requires %i arguments, \
                      but none is given.\n\
                      You may undefine the macro with #undef."
	       name (List.length arg_names))
  with
      Not_found -> None


and print_ast (env : env) (ch : out_obj_channel) (x : ast) =
  match x with
      `Ident (loc, (name, opt_args), orig_args) ->
	(match opt_args with
	     None ->
	       ch # output_string (
		 match subst_const env loc name with
		     None -> name
		   | Some s -> s
	       )
	   | Some args ->
	       let string_args = List.map (to_string env) args in
	       match subst_app env loc name string_args with
		   None -> 
		     ch # output_string name;
		     ch # output_string orig_args
		 | Some s ->
		     ch # output_string s
	);
	env

    | `Def (loc, name, body) ->
	let value = to_string env body in
	String_map.add name (`Constant value) env

    | `Defun (loc, name, arg_names, body) ->
	String_map.add name (`Function (env, arg_names, body)) env

    | `Undef (loc, name) ->
	String_map.remove name env

    | `Include (loc, lz) ->
	print env ch (Lazy.force lz)

    | `Cond (loc, cond, if_true, if_false) ->
	let x =
	  if eval_bool env cond then if_true
	  else if_false
	in
	print env ch x

    | `Error (loc, msg) -> error loc msg

    | `Warning (loc, msg) -> warning loc msg; env

    | `Text (loc, s) -> ch # output_string s; env


and print (env : env) (ch : out_obj_channel) l =
  List.fold_left (fun env x -> print_ast env ch x) env l

and to_string env body =
  let buf = Buffer.create 1000 in
  let ch = new output_buffer buf in
  ignore (print env ch body);
  Buffer.contents buf
