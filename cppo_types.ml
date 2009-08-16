(* $Id$ *)

open Printf
open Lexing

module String_set = Set.Make (String)
module String_map = Map.Make (String)

type loc = position * position

type bool_expr =
    [ `Defined of string
    | `Not of bool_expr (* not *)
    | `And of (bool_expr * bool_expr) (* && *)
    | `Or of (bool_expr * bool_expr) (* || *)
    | `Eq of (arith_expr * arith_expr) (* = *)
    | `Lt of (arith_expr * arith_expr) (* < *)
    | `Gt of (arith_expr * arith_expr) (* > *)
	(* syntax for additional operators: <=, >= *)
    ]

and arith_expr = (* signed int64 *)
    [ `Int of int64
    | `Ident of (loc * string)
	(* must be bound to a valid int literal.
	   Expansion of macro functions is not supported. *)

    | `Neg of arith_expr (* - *)
    | `Add of (arith_expr * arith_expr) (* + *)
    | `Sub of (arith_expr * arith_expr) (* - *)
    | `Mul of (arith_expr * arith_expr) (* * *)
    | `Div of (loc * arith_expr * arith_expr) (* / *)
    | `Mod of (loc * arith_expr * arith_expr) (* mod *)

    (* Bitwise operations on 64 bits *)
    | `Lnot of arith_expr (* lnot *)
    | `Lsl of (arith_expr * arith_expr) (* lsl *)
    | `Lsr of (arith_expr * arith_expr) (* lsr *)
    | `Land of (arith_expr * arith_expr) (* land *)
    | `Lor of (arith_expr * arith_expr) (* lor *)
    | `Lxor of (arith_expr * arith_expr) (* lxor *)
    ]

and ast =
    [ `Ident of (loc * (string * ast list list option) * string)
	(* (loc, (ident, args), original args string) *)
    | `Def of (loc * string * ast list)
    | `Defun of (loc * string * string list * ast list)
    | `Undef of (loc * string)
    | `Include of (loc * ast list lazy_t)
    | `Cond of (loc * bool_expr * ast list * ast list)
    | `Error of (loc * string)
    | `Warning of (loc * string)
    | `Text of (loc * string) ]



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
    sprintf "%s\nWarning: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg

let make_line_directive ?(fname = true) pos =
  let spaces = String.make (pos.pos_cnum - pos.pos_bol) ' ' in
  if fname then
    sprintf "# %i %S\n%s" pos.pos_lnum pos.pos_fname spaces
  else
    sprintf "# %i\n%s" pos.pos_lnum spaces

