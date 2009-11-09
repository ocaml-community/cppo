(* $Id$ *)

open Printf

let () =
  let files = ref [] in
  let options = [] in
  let msg = sprintf "Usage: %s [file1 [file2 ...]]" Sys.argv.(0) in
  let add_file s = files := s :: !files in
  Arg.parse options add_file msg;
  
  let in_channels = 
    match List.rev !files with
	[] -> [ "", (fun () -> stdin), (fun () -> ()) ]
      | l -> 
	  List.map (
	    fun file -> 
	      let ic = lazy (open_in file) in
	      file,
	      (fun () -> Lazy.force ic),
	      (fun () -> close_in (Lazy.force ic))
	  ) l
  in

  let env = Cppo_eval.M.empty in
  let buf = Buffer.create 10_000 in
  let _env = Cppo_eval.include_channels buf env in_channels in
  print_string (Buffer.contents buf);
  flush stdout


