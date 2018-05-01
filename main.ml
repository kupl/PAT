open Lang
open Solver
open Options
open Helper

let main () =
  let usageMsg = "./main.native -input filename" in
  let _ = Arg.parse options (fun s->()) usageMsg in
  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let examples = Parser.input Lexer.start lexbuf in
  (* let examples = fix onestep_trim_exs examples in *) (* TODO : fix trim function *) 
  let rownums = rows_of_exs examples in
  let colnums = cols_of_exs examples in
  let _ = 
    if not !Options.exp && not !Options.demo then (* normal case *) 
      let _ = print_endline "========== EXAMPLES ==========" in
      let _ = print_examples examples in
              print_endline "========== PROCESSING ==========" in 
  let t0 = Sys.time () in
  let pgm = Synthesizer.synthesize examples rownums colnums in 
  let t1 = Sys.time () in
    match pgm with
    | None -> print_string ("Synthesis Failed")
    | Some pgm ->
      if (!Options.exp) then 
        print_string (string_of_float (t1 -. t0)) else
      if (!Options.demo) then
        print_endline (c_pretty_pgm pgm)
      else
        let _ = print_endline "========== EXAMPLES ==========" in
        let _ = print_examples examples in
        let _ = print_endline "========== PROGRAM ==========" in
        let _ = print_endline (pretty_pgm pgm) in
        let _ = print_endline "========== REPORT ==========" in
        let _ = print_endline ("Iter : " ^ string_of_int !Synthesizer.iter) in
                print_endline ("Time : " ^ string_of_float (t1 -. t0) ^ "seconds\n")

let _ = main () 
