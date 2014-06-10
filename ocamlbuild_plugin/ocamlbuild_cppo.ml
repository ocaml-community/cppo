
open Ocamlbuild_plugin

let dispatcher = function
  | After_rules -> begin
      let dep = "%(name).cppo.ml" in
      let prod1 = "%(name: <*> and not <*.cppo>).ml" in
      let prod2 = "%(name: <**/*> and not <**/*.cppo>).ml" in
      let f prod env _build =
        let dep = env dep in
        let prod = env prod in
        let tags = tags_of_pathname prod ++ "cppo" in
        Cmd (S[A "cppo"; T tags; S [A "-o"; P prod]; P dep ])
      in
      rule "cppo1" ~dep ~prod:prod1 (f prod1) ;
      rule "cppo2" ~dep ~prod:prod2 (f prod2) ;
      pflag ["cppo"] "cppo_D" (fun s -> S [A "-D"; A s]) ;
      pflag ["cppo"] "cppo_U" (fun s -> S [A "-U"; A s]) ;
      pflag ["cppo"] "cppo_I" (fun s ->
        if Pathname.is_directory s then S [A "-I"; P s]
        else S [A "-I"; P (Pathname.dirname s)]
      ) ;
      pdep ["cppo"] "cppo_I" (fun s ->
        if Pathname.is_directory s then [] else [s]) ;
      flag ["cppo"; "cppo_q"] (A "-q") ;
      flag ["cppo"; "cppo_s"] (A "-s") ;
      flag ["cppo"; "cppo_s"] (A "-n") ;
      pflag ["cppo"] "cppo_x" (fun s -> S [A "-x"; A s])
    end
  | _ -> ()
