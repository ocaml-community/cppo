(* $Id$ *)

open Printf

let () =
  let files = ref [] in
  let header = ref [] in
  let incdirs = ref [] in
  let out_file = ref None in
  let preserve_quotations = ref false in
  let show_exact_locations = ref false in
  let show_no_locations = ref false in
  let options = [
    "-D", Arg.String (fun s -> header := ("#define " ^ s ^ "\n") :: !header),
    "DEF
          Equivalent of interpreting '#define DEF' before processing the
          input";

    "-U", Arg.String (fun s -> header := ("#undef " ^ s ^ "\n") :: !header),
    "IDENT
          Equivalent of interpreting '#undef IDENT' before processing the
          input";

    "-I", Arg.String (fun s -> incdirs := s :: !incdirs),
    "DIR
          Add directory DIR to the search path for included files";

    "-o", Arg.String (fun s -> out_file := Some s),
    "FILE
          Output file";

    "-q", Arg.Set preserve_quotations,
    "
          Identify and preserve camlp4 quotations";

    "-s", Arg.Set show_exact_locations,
    "
          Output line directives pointing to the exact source location of 
          each token, including those coming from the body of macro 
          definitions.  This behavior is off by default.";

    "-n", Arg.Set show_no_locations,
    "
          Do not output any line directive other than those found in the 
          input (overrides -s).";

    "-version", Arg.Unit (fun () ->
			    print_endline Cppo_version.cppo_version;
			    exit 0),
    "
          Print the version of the program and exit.";
  ]
  in
  let msg = sprintf "\
Usage: %s [OPTIONS] [FILE1 [FILE2 ...]]
Options:" Sys.argv.(0) in
  let add_file s = files := s :: !files in
  Arg.parse options add_file msg;
  
  let inputs =
    let preliminaries =
      match List.rev !header with
	  [] -> []
	| l ->
	    let s = String.concat "" l in
	    [ Sys.getcwd (),
	      "<command line>", 
	      (fun () -> Lexing.from_string s),
	      (fun () -> ()) ]
    in
    let main =
      match List.rev !files with
	  [] -> [ Sys.getcwd (),
		  "<stdin>", 
		  (fun () -> Lexing.from_channel stdin),
		  (fun () -> ()) ]
	| l -> 
	    List.map (
	      fun file -> 
		let ic = lazy (open_in file) in
		Filename.dirname file,
		file,
		(fun () -> Lexing.from_channel (Lazy.force ic)),
		(fun () -> close_in (Lazy.force ic))
	    ) l
    in
    preliminaries @ main
  in

  let env = Cppo_eval.builtin_env in
  let buf = Buffer.create 10_000 in
  let _env =
    try Cppo_eval.include_inputs
      ~preserve_quotations: !preserve_quotations
      ~incdirs: (List.rev !incdirs)
      ~show_exact_locations: !show_exact_locations
      ~show_no_locations: !show_no_locations
      buf env inputs
    with Cppo_types.Cppo_error msg ->
      eprintf "%s\n%!" msg;
      exit 1
  in
  match !out_file with
      None ->
	print_string (Buffer.contents buf);
	flush stdout
    | Some file ->
	let oc = open_out file in
	output_string oc (Buffer.contents buf);
	close_out oc
