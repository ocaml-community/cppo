(* Simple wrapper around system "diff" commands for portability. *)

open Printf

let () =
  let f1 = Filename.quote Sys.argv.(1)
  and f2 = Filename.quote Sys.argv.(2) in
  match Sys.os_type with
  | "Unix" ->
     exit(Sys.command(sprintf "diff -u %s %s" f1 f2))
  | "Win32" | "Cygwin" ->
     let code = Sys.command(sprintf "FC %s %s" f1 f2) in
     printf "FC %s %s returned %d\n" f1 f2 code;
     exit(code)
  | _ -> assert false
