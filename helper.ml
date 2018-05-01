open Lang

let rows_of_ex : example -> int
= fun example -> 
  BatString.fold_left (fun acc ch -> 
    if ch = '\n' then acc+1 else acc
  ) 0 example

let cols_of_ex : example -> int list 
= fun example -> 
  let (_,r) = 
    BatString.fold_left (fun (cnt,acc) ch -> 
      if ch = '\n' then (0,acc@[cnt]) else (cnt+1,acc)
    ) (0,[]) example in
  r

let rows_of_exs : example list -> int list
= fun examples ->
  List.fold_left (fun acc ex -> acc@[rows_of_ex ex]) [] examples

let cols_of_exs : example list -> int list list
= fun examples ->
  List.fold_left (fun acc ex -> acc@[cols_of_ex ex]) [] examples

let print_examples : example list -> unit
= fun examples ->
  let _ =
    List.fold_left (fun cnt ex ->
      print_endline ("example " ^ string_of_int cnt);
      print_string ex; cnt+1
    ) 1 examples in ()

let newline_in_last' : example -> example
= fun ex ->
  if BatString.ends_with ex "\n" then ex
  else ex ^ "\n" 

let newline_in_last : example list -> example list
= fun examples ->
  List.map newline_in_last' examples

(* \n\n -> \n, ' '\n -> \n  *)

let trim_newline : example -> example
= fun ex ->
  let ex' = BatString.nreplace ~str:ex ~sub:"\n\n" ~by:"\n" in
  let ex'' = BatString.nreplace ~str:ex' ~sub:" \n" ~by:"\n" in
    ex''

let trim_newline : example list -> example list
= fun examples ->
  List.map trim_newline examples 

let onestep_trim_exs examples = 
  examples 
  |> newline_in_last
  |> trim_newline

let rec fix f x = 
  let x' = f x in
    if x' = x then x'
    else fix f x'

let print_not_pruned_states : int list -> int list list -> state -> unit
= fun rownums colnums state ->
  let _ = print_endline "- state not being pruned -" in
  let _ = print_endline (to_string state) in
  let _ = print_endline "- reasons for not being pruned -" in
  let _ = 
    List.iter2 (fun rownum colnum ->
      print_endline ("rownum : " ^ string_of_int rownum); 
      print_endline (Abs.pretty_run rownum colnum state)
    ) rownums colnums in 
  let _ = print_endline "" in 
    ()

let print_pruned_states : int list -> int list list -> state -> unit
= fun rownums colnums state ->
  let _ = print_endline "- state pruned -" in
  let _ = print_endline (to_string state) in
  let _ = print_endline "- reasons for being pruned -" in
  let _ = 
    List.iter2 (fun rownum colnum ->
      print_endline ("rownum : " ^ string_of_int rownum); 
      print_endline (Abs.pretty_run rownum colnum state)
    ) rownums colnums in 
  let _ = print_endline "" in 
    ()
