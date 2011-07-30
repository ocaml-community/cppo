(* $Id$ *)

open Printf

open Cppo_types

module S = Set.Make (String)
module M = Map.Make (String)

let empty_env = M.empty

let builtins = [
  "__FILE__", (fun env -> `Special);
  "__LINE__", (fun env -> `Special);
  "STRINGIFY", (fun env ->
                  `Defun (dummy_loc, "STRINGIFY",
                          ["x"],
                          [`Stringify (`Ident (dummy_loc, "x", None))],
                          env)
               );
  "CONCAT", (fun env ->
               `Defun (dummy_loc, "CONCAT",
                       ["x";"y"],
                       [`Concat (`Ident (dummy_loc, "x", None),
                                 `Ident (dummy_loc, "y", None))],
                       env)
            );
]

let is_reserved s =
  List.exists (fun (s', _) -> s = s') builtins

let builtin_env =
  List.fold_left (fun env (s, f) -> M.add s (f env) env) M.empty builtins

let line_directive buf prev_file pos =
  let file = pos.Lexing.pos_fname in
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
    Buffer.add_char buf '\n';
  (match prev_file with
       Some s when s = file ->
	 bprintf buf "# %i\n"
	   pos.Lexing.pos_lnum
     | _ ->
	 bprintf buf "# %i %S\n"
	   pos.Lexing.pos_lnum
	   pos.Lexing.pos_fname
  );
  bprintf buf "%s" (String.make (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ' ')
    
let rec add_sep sep last = function
    [] -> [ last ]
  | [x] -> [ x; last ]
  | x :: l -> x :: sep :: add_sep sep last l 


let trim s =
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
  try Some (Int64.of_string (trim s))
  with _ -> None

let remove_space l =
  List.filter (function `Text (_, true, _) -> false | _ -> true) l

let trim_and_compact buf s =
  let started = ref false in
  let need_space = ref false in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        ' ' | '\t' | '\n' | '\r' ->
          if !started then
            need_space := true
      | c ->
          if !need_space then
            Buffer.add_char buf ' ';
          (match c with 
               '\"' -> Buffer.add_string buf "\\\""
             | '\\' -> Buffer.add_string buf "\\\\"
             | c -> Buffer.add_char buf c);
          started := true;
          need_space := false
  done

let stringify buf s =
  Buffer.add_char buf '\"';
  trim_and_compact buf s;
  Buffer.add_char buf '\"'

let trim_and_compact_string s =
  let buf = Buffer.create (String.length s) in
  trim_and_compact buf s;
  Buffer.contents buf

let is_ident s =
  let len = String.length s in
  len > 0
  && 
    (match s.[0] with
         'A'..'Z' | 'a'..'z' -> true
       | '_' when len > 1 -> true
       | _ -> false)
  &&
    (try
       for i = 1 to len - 1 do
         match s.[i] with
             'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> ()
           | _ -> raise Exit
       done;
       true
     with Exit ->
       false)

let concat loc x y =
  let s = trim_and_compact_string x ^ trim_and_compact_string y in
  if not (s = "" || is_ident s) then
    error loc
      (sprintf "CONCAT() does not expand into a valid identifier nor \
                into whitespace:\n%S" s)
  else
    if s = "" then " "
    else " " ^ s ^ " "

let rec eval_int env (x : arith_expr) : int64 =
  match x with
      `Int x -> x
    | `Ident (loc, name) ->
	let l =
	  try
	    match M.find name env with
		`Def (_, _, l, _) -> l
	      | `Defun _ -> 
		  error loc (sprintf "%S expects arguments" name)
	      | `Special -> assert false
	  with Not_found -> error loc (sprintf "Undefined identifier %S" name)
	in
	(try
	   match remove_space l with
	       [ `Ident (loc, name, None) ] -> 
		 eval_int env (`Ident (loc, name))
	     | _ ->
		 let text =
		   List.map (
		     function
			 `Text (_, is_space, s) -> s
		       | _ ->
			   error loc
			     (sprintf
				"Identifier %S is not bound to a constant"
				name)
		   ) l
		 in
		 let s = String.concat "" text in
		 (match int_of_string_with_space s with
		      None -> 
			error loc 
			  (sprintf
			     "Identifier %S is not bound to an int literal"
			     name)
		    | Some n -> n
		 )
	 with Cppo_error s ->
	   error loc (sprintf "Identifier %S does not expand to an int:\n%s"
			name s)
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
	let shift =
	  if shift >= 64L then 64L
	  else if shift <= -64L then -64L
	  else shift
	in
	Int64.shift_left n (Int64.to_int shift)

    | `Lsr (a, b) ->
	let n = eval_int env a in
	let shift = eval_int env b in
	let shift =
	  if shift >= 64L then 64L
	  else if shift <= -64L then -64L
	  else shift
	in
	Int64.shift_right_logical n (Int64.to_int shift)

    | `Asr (a, b) ->
	let n = eval_int env a in
	let shift = eval_int env b in
	let shift =
	  if shift >= 64L then 64L
	  else if shift <= -64L then -64L
	  else shift
	in
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

    
type globals = {
  call_loc : Cppo_types.loc;
    (* location used to set the value of
       __FILE__ and __LINE__ global variables *)

  mutable buf : Buffer.t;
    (* buffer where the output is written *)

  included : S.t;
    (* set of already-included files *)

  require_location : bool ref;
    (* whether a line directive should be printed before outputting the next
       token *)

  last_file_loc : string option ref;
    (* used to test whether a line directive should include the file name *)

  show_exact_locations : bool;
    (* whether line directives should be printed even for expanded macro
       bodies *)

  enable_loc : bool ref;
    (* whether line directives should be printed *)

  g_preserve_quotations : bool;
    (* identify and preserve camlp4 quotations *)

  incdirs : string list;
    (* directories for finding included files *)

  current_directory : string;
    (* directory containing the current file *)

  extensions : (string, Cppo_command.command_template) Hashtbl.t;
    (* mapping from extension ID to pipeline command *)
}

        

let parse ~preserve_quotations file lexbuf =
  let lexer_env = Cppo_lexer.init ~preserve_quotations file lexbuf in
  try
    Cppo_parser.main (Cppo_lexer.line lexer_env) lexbuf
  with
      Parsing.Parse_error ->
	error (Cppo_lexer.loc lexbuf) "syntax error"
    | Cppo_types.Cppo_error _ as e ->
	raise e
    | e ->
	error (Cppo_lexer.loc lexbuf) (Printexc.to_string e)

let plural n =
  if abs n <= 1 then ""
  else "s"


let maybe_print_location g pos =
  if !(g.enable_loc) then
    let prev_file = !(g.last_file_loc) in
    let file = pos.Lexing.pos_fname in
    if !(g.require_location) then (
      line_directive g.buf prev_file pos;
      g.last_file_loc := Some file
    )

let expand_ext g loc id data =
  let cmd_tpl =
    try Hashtbl.find g.extensions id
    with Not_found ->
      error loc (sprintf "Undefined extension %s" id)
  in
  let p1, p2 = loc in
  let file = p1.Lexing.pos_fname in
  let first = p1.Lexing.pos_lnum in
  let last = p2.Lexing.pos_lnum in
  let cmd = Cppo_command.subst cmd_tpl file first last in
  Unix.putenv "CPPO_FILE" file;
  Unix.putenv "CPPO_FIRST_LINE" (string_of_int first);
  Unix.putenv "CPPO_LAST_LINE" (string_of_int last);
  let (ic, oc) as p = Unix.open_process cmd in
  output_string oc data;
  close_out oc;
  (try
     while true do
       bprintf g.buf "%s\n" (input_line ic)
     done
   with End_of_file -> ()
  );
  match Unix.close_process p with
      Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        failwith (sprintf "Command %S exited with status %i" cmd n)
    | _ ->
        failwith (sprintf "Command %S failed" cmd)

let rec include_file g loc rel_file env =
  let file =
    if not (Filename.is_relative rel_file) then
      if Sys.file_exists rel_file then
	rel_file
      else
	error loc (sprintf "Included file %S does not exist" rel_file)
    else
      try
	let dir = 
	  List.find (
	    fun dir ->
	      let file = Filename.concat dir rel_file in
	      Sys.file_exists file
	  ) (g.current_directory :: g.incdirs)
	in
	if dir = Filename.current_dir_name then
	  rel_file
	else
	  Filename.concat dir rel_file
      with Not_found -> 
	error loc (sprintf "Cannot find included file %S" rel_file)
  in
  if S.mem file g.included then
    failwith (sprintf "Cyclic inclusion of file %S" file)
  else
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    let l = parse ~preserve_quotations:g.g_preserve_quotations file lexbuf in
    close_in ic;
    expand_list { g with
		    included = S.add file g.included;
		    current_directory = Filename.dirname file
		} env l

and expand_list ?(top = false) g env l =
  List.fold_left (expand_node ~top g) env l

and expand_node ?(top = false) g env0 x =
  match x with
      `Ident (loc, name, opt_args) ->

	let def =
	  try Some (M.find name env0)
	  with Not_found -> None
	in
	let g =
	  if top && def <> None then
	    { g with call_loc = loc }
	  else g
	in

	let enable_loc0 = !(g.enable_loc) in

	if def <> None then (
	  g.require_location := true;

	  if not g.show_exact_locations then (
	    (* error reports will point more or less to the point
	       where the code is included rather than the source location
	       of the macro definition *)
	    maybe_print_location g (fst loc);
	    g.enable_loc := false
	  )
	);

	let env =
	  match def, opt_args with
	      None, None ->
		expand_node g env0 (`Text (loc, false, name))
	    | None, Some args ->
		let with_sep = 
		  add_sep
		    [`Text (loc, false, ",")]
		    [`Text (loc, false, ")")]
		    args in
		let l =
		  `Text (loc, false, name ^ "(") :: List.flatten with_sep in
		expand_list g env0 l
		  
	    | Some (`Defun (_, _, arg_names, _, _)), None ->
		error loc 
		  (sprintf "%S expects %i arguments but is applied to none." 
		     name (List.length arg_names))
		  
	    | Some (`Def _), Some l ->
		error loc 
		  (sprintf "%S expects no arguments" name)
		  
	    | Some (`Def (_, _, l, env)), None ->
		ignore (expand_list g env l);
		env0
		  
	    | Some (`Defun (_, _, arg_names, l, env)), Some args ->
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
		    (sprintf "%S expects %i argument%s but is applied to \
                              %i argument%s."
		       name argc (plural argc) n (plural n))
		else
		  let app_env =
		    List.fold_left2 (
		      fun env name l ->
			M.add name (`Def (loc, name, l, env0)) env
		    ) env arg_names args
		  in
		  ignore (expand_list g app_env l);
		  env0
		    
	    | Some `Special, _ -> assert false
	in

	if def = None then
	  g.require_location := false
	else
	  g.require_location := true;

	(* restore initial setting *)
	g.enable_loc := enable_loc0;

	env


    | `Def (loc, name, body)-> 
	g.require_location := true;
	if M.mem name env0 then
	  error loc (sprintf "%S is already defined" name)
	else
	  M.add name (`Def (loc, name, body, env0)) env0

    | `Defun (loc, name, arg_names, body) ->
	g.require_location := true;
	if M.mem name env0 then
	  error loc (sprintf "%S is already defined" name)
	else	
	  M.add name (`Defun (loc, name, arg_names, body, env0)) env0

    | `Undef (loc, name) ->
	g.require_location := true;
	if is_reserved name then
	  error loc 
	    (sprintf "%S is a built-in variable that cannot be undefined" name)
	else
	  M.remove name env0

    | `Include (loc, file) ->
	g.require_location := true;
	let env = include_file g loc file env0 in
	g.require_location := true;
	env

    | `Ext (loc, id, data) ->
        g.require_location := true;
        expand_ext g loc id data;
        g.require_location := true;
        g.last_file_loc := None;
	env0

    | `Cond (loc, test, if_true, if_false) ->
	let l =
	  if eval_bool env0 test then if_true
	  else if_false
	in
	g.require_location := true;
	let env = expand_list g env0 l in
	g.require_location := true;
	env

    | `Error (loc, msg) ->
	error loc msg

    | `Warning (loc, msg) ->
	warning loc msg;
	env0

    | `Text (loc, is_space, s) ->
	if not is_space then (
	  maybe_print_location g (fst loc);
	  g.require_location := false
	);
	Buffer.add_string g.buf s;
	env0

    | `Seq l ->
	expand_list g env0 l

    | `Stringify x ->
        let enable_loc0 = !(g.enable_loc) in
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        stringify buf0 (Buffer.contents local_buf);
        g.buf <- buf0;
        g.enable_loc := enable_loc0;
        env0

    | `Concat (x, y) ->
        let enable_loc0 = !(g.enable_loc) in
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        let xs = Buffer.contents local_buf in
        Buffer.clear local_buf;
        ignore (expand_node g env0 y);
        let ys = Buffer.contents local_buf in
        let s = concat g.call_loc xs ys in
        Buffer.add_string buf0 s;
        g.buf <- buf0;
        g.enable_loc := enable_loc0;
        env0

    | `Line (loc, opt_file, n) ->
	(* printing a line directive is not strictly needed *)
	(match opt_file with
	     None ->
	       maybe_print_location g (fst loc);
	       bprintf g.buf "\n# %i\n" n
	   | Some file ->
	       bprintf g.buf "\n# %i %S\n" n file
	);
	(* printing the location next time is needed because it just changed *)
	g.require_location := true;
	env0

    | `Current_line loc ->
	maybe_print_location g (fst loc);
	g.require_location := true;
	let pos, _ = g.call_loc in
	bprintf g.buf " %i " pos.Lexing.pos_lnum;
	env0

    | `Current_file loc ->
	maybe_print_location g (fst loc);
	g.require_location := true;
	let pos, _ = g.call_loc in
	bprintf g.buf " %S " pos.Lexing.pos_fname;
	env0

	  


let include_inputs
    ~extensions
    ~preserve_quotations
    ~incdirs
    ~show_exact_locations
    ~show_no_locations
    buf env l =

  let enable_loc = not show_no_locations in
  List.fold_left (
    fun env (dir, file, open_, close) ->
      let l = parse ~preserve_quotations file (open_ ()) in
      close ();
      let g = {
	call_loc = dummy_loc;
        buf = buf;
	included = S.empty;
	require_location = ref true;
	last_file_loc = ref None;
	show_exact_locations = show_exact_locations;
	enable_loc = ref enable_loc;
	g_preserve_quotations = preserve_quotations;
	incdirs = incdirs;
	current_directory = dir;
        extensions = extensions;
      }
      in
      expand_list ~top:true { g with included = S.add file g.included } env l
  ) env l
