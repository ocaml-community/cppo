(* $Id$ *)

open Printf

let () =
  let files = ref [] in
  let header = ref [] in
  let incdirs = ref [] in
  let out_file = ref None in
  let preserve_quotations = ref false in
  let options = [
    "-D", Arg.String (fun s -> header := ("#define " ^ s ^ "\n") :: !header),
    "DEF
          Equivalent of interpreting #define DEF before processing the input";

    "-U", Arg.String (fun s -> header := ("#undef " ^ s ^ "\n") :: !header),
    "IDENT
          Equivalent of interpreting #undef IDENT before processing the input";

    "-I", Arg.String (fun s -> incdirs := s :: !incdirs),
    "DIR
          Add directory DIR to the search path";

    "-o", Arg.String (fun s -> out_file := Some s),
    "FILE
          Output file";

    "-q", Arg.Set preserve_quotations,
    "
          Identify and preserve camlp4 quotations";
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
	let oc = open_out_bin file in
	output_string oc (Buffer.contents buf);
	close_out oc
