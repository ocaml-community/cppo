(* $Id$ *)

open Printf
open Lexing

module String_set = Set.Make (String)
module String_map = Map.Make (String)

type macro_value =
    [ `Constant of string
    | `Function of (string list * macro_token list) ]
      
and macro_token =
    [ `Var of string
    | `Text of string ]

type macro_defs = macro_value String_map.t
type string_defs = string String_map.t

type loc = position * position

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
