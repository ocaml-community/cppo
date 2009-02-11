%{
  open Printf
  open Cppo_types

  let print = print_string

  let rhs_loc n = (Parsing.rhs_start_pos n, Parsing.rhs_end_pos n)
                    
  let unclosed opening_name opening_num closing_name closing_num =
    let msg = 
      sprintf "%s:\nSyntax error: '%s' expected.\n\
               %s:\nThis '%s' might be unmatched."
        (string_of_loc (rhs_loc closing_num)) closing_name
        (string_of_loc (rhs_loc opening_num)) opening_name in
    failwith msg

  let syntax_error s num =
    error (rhs_loc num) s
%}
%token < Cppo_types.loc * string > TEXT
%token < Cppo_types.loc * Cppo_types.macro_defs * string > IDENT
%token < Cppo_types.loc * string > OPEN CLOSE COMMA
%token < Cppo_types.loc > EOF
%start all
%type < string list > all
%%
all:
  top all { $1 @ $2 }
| EOF     { [] }

top:
  TEXT { [ snd $1 ] }
| IDENT OPEN args CLOSE 
      { let loc, defs, name = $1 in
	let arg_list, arg_text = $3 in
	try 
	  match String_map.find name (defs : macro_defs) with
	      `Constant _ -> 
		(* this is not an error for cpp, but really error-prone. *)
		error loc
		  (sprintf "Macro %S passed %i arguments, \
                            but it is a constant.\n\
                            Place it between parentheses if needed."
		     name (List.length arg_list))
	    | `Function (arg_names, l) ->
		let arg_n = List.length arg_names in
		let arg_c = List.length arg_list in
		if arg_n < arg_c then
		  error loc 
		    (sprintf "Macro %S passed %i arguments, \
                              but takes just %i."
		       name arg_c arg_n)
		else if arg_c < arg_n then
		  error loc 
		    (sprintf "Macro %S requires %i arguments, \
                              but only %i given."
		       name arg_n arg_c)
		else
		  (* unlike cpp we restore the environment from the closure. *)
		  let defs : string_defs =
		    List.fold_left2 (fun defs k s -> String_map.add k s defs) 
		      String_map.empty arg_names arg_list
		  in
		  List.map (
		    function
			`Var name ->
			  (try String_map.find name defs
			   with Not_found -> assert false)
		      | `Text s ->
			  s
		  ) l
	with
	    Not_found ->
	      name :: snd $2 :: arg_text @ [snd $4]
      }

| IDENT OPEN args error { unclosed "(" 2 ")" 4 }

| IDENT  { let loc, defs, name = $1 in
	   match String_map.find name defs with
	       `Constant s -> [ s ]
	     | `Function (arg_names, _) ->
		 (* this is not an error for cpp, but really error-prone. *)
		 error loc
		   (sprintf "Macro %S requires %i arguments, \
                             but none is given."
		      name (List.length arg_names))
	 }

;

args:
  top COMMA args   { let arg_list, arg_text = $3 in
		     let l = $1 in
		     (l @ arg_list), (l @ snd $2 :: arg_text) }
| top              { ($1, $1) }
;
