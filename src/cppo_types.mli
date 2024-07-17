type loc = Lexing.position * Lexing.position

exception Cppo_error of string

(* The name of a macro. *)
type macro =
  string

type bool_expr =
    [ `True
    | `False
    | `Defined of macro
    | `Not of bool_expr (* not *)
    | `And of (bool_expr * bool_expr) (* && *)
    | `Or of (bool_expr * bool_expr) (* || *)
    | `Eq of (arith_expr * arith_expr) (* = *)
    | `Lt of (arith_expr * arith_expr) (* < *)
    | `Gt of (arith_expr * arith_expr) (* > *)
        (* syntax for additional operators: <>, <=, >= *)
    ]

and arith_expr = (* signed int64 *)
    [ `Int of int64
    | `Ident of (loc * string)
        (* must be bound to a valid int literal.
           Expansion of macro functions is not supported. *)

    | `Tuple of (loc * arith_expr list)
        (* tuple of 2 or more elements guaranteed by the syntax *)

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
    | `Asr of (arith_expr * arith_expr) (* asr *)
    | `Land of (arith_expr * arith_expr) (* land *)
    | `Lor of (arith_expr * arith_expr) (* lor *)
    | `Lxor of (arith_expr * arith_expr) (* lxor *)
    ]

type node =
    [ `Ident of (loc * string * actuals)
         (* the list [actuals] is empty if and only if no parentheses
            are used at this macro invocation site. *)
    | `Def of (loc * macro * formals * body)
         (* the list [formals] is empty if and only if no parentheses
            are used at this macro definition site. *)
    | `Undef of (loc * macro)
    | `Include of (loc * string)
    | `Ext of (loc * string * string)
    | `Cond of (loc * bool_expr * node list * node list)
    | `Error of (loc * string)
    | `Warning of (loc * string)
    | `Text of (loc * bool * string) (* bool is true for space tokens *)
    | `Seq of (loc * node list)
    | `Stringify of node
    | `Capitalize of node
    | `Concat of (node * node)
    | `Line of (loc * string option * int)
    | `Current_line of loc
    | `Current_file of loc ]

(* One formal macro parameter. *)
and formal =
  string

(* A tuple of formal macro parameters. *)
and formals =
  formal list

(* One actual macro argument. *)
and actual =
  node list

(* A tuple of actual macro arguments. *)
and actuals =
  actual list

(* The body of a macro definition. *)
and body =
  node list

val dummy_loc : loc

val error : loc -> string -> _

val warning : loc -> string -> unit

val flatten_nodes : node list -> node list
