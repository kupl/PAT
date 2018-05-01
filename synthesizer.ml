open Lang
open Solver
open Abs
open Helper

let iter = ref 0

let start_time = ref (Sys.time ())

module Workset = struct
  type work = pgm

  module OrderedType = struct
    type t = work 
    let compare p1 p2 =
      let c1,c2 = cost p1, cost p2 in
        if c1=c2 then 0 else
        if c1>c2 then 1
        else -1
  end

  module Heap = BatHeap.Make (OrderedType)
  
  (* type of workset : (remaining data) heap * (visited) string set *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)
  
  let explored : pgm -> t -> bool
  = fun pgm (_,sset) -> BatSet.mem (to_string pgm) sset
  
  let add : work -> t -> t
  = fun pgm (heap,sset) -> 
    if explored pgm (heap,sset) then (heap,sset)
    else
      (Heap.add pgm heap, BatSet.add (to_string pgm) sset)
  
  let choose : t -> (work * t) option
  = fun (heap,sset) ->
    try 
      let elem = Heap.find_min heap in
        Some (elem, (Heap.del_min heap, sset))
    with
      | _ -> None
  
  let workset_info : t -> string
  = fun (heap,sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

(* Collect each type of holes in a state *)

let aholes : state -> aexp BatSet.t
= fun (_,_,c) ->
  let rec aholes_froma : aexp -> aexp BatSet.t
  = fun aexp ->
    match aexp with
    | AHole id -> BatSet.singleton (AHole id)
    | _ -> BatSet.empty in
  let rec aholes_fromb : bexp -> aexp BatSet.t
  = fun bexp ->
    match bexp with
    | EqJ a-> aholes_froma a
    | Or (b1,b2) -> BatSet.union (aholes_fromb b1) (aholes_fromb b2)
    | _ -> BatSet.empty in
  let rec aholes_fromp : print -> aexp BatSet.t
  = fun print ->
    match print with
    | If (b,_,_) -> aholes_fromb b
    | _ -> BatSet.empty in
  let rec aholes_fromc : col -> aexp BatSet.t
  = fun col ->
    match col with
    | For (_,a,l) -> BatSet.union (aholes_froma a) (aholes_fromp l)
    | ColSeq (c1,c2) -> BatSet.union (aholes_fromc c1) (aholes_fromc c2)
    | _ -> BatSet.empty
  in aholes_fromc c

let bholes : state -> bexp BatSet.t
= fun (_,_,c) ->
  let rec bholes_fromb : bexp -> bexp BatSet.t
  = fun bexp ->
    match bexp with
    | Or (b1,b2) -> BatSet.union (bholes_fromb b1) (bholes_fromb b2)
    | BHole id -> BatSet.singleton (BHole id) 
    | _ -> BatSet.empty in
  let rec bholes_fromp : print -> bexp BatSet.t
  = fun print ->
    match print with
    | If (b,_,_) -> bholes_fromb b
    | _ -> BatSet.empty in
  let rec bholes_fromc : col -> bexp BatSet.t
  = fun col ->
    match col with
    | For (_,_,p) -> bholes_fromp p
    | ColSeq (c1,c2) -> BatSet.union (bholes_fromc c1) (bholes_fromc c2)
    | _ -> BatSet.empty 
  in bholes_fromc c

let pholes : state -> print BatSet.t 
= fun (_,_,c) ->
  let rec pholes_fromp : print -> print BatSet.t
  = fun print ->
    match print with
    | PHole id -> BatSet.singleton (PHole id) 
    | _ -> BatSet.empty in
  let rec pholes_fromc : col -> print BatSet.t
  = fun col ->
    match col with
    | For (_,_,p) -> pholes_fromp p 
    | ColSeq (c1,c2) -> BatSet.union (pholes_fromc c1) (pholes_fromc c2)
    | _ -> BatSet.empty
  in pholes_fromc c 

let choles : state -> col BatSet.t
= fun (_,_,c) ->
  let rec choles_fromc : col -> col BatSet.t
  = fun col ->
    match col with
    | ColSeq (c1,c2) -> BatSet.union (choles_fromc c1) (choles_fromc c2)
    | CHole id -> BatSet.singleton (CHole id) 
    | _ -> BatSet.empty
  in choles_fromc c 

(* Generate candidates for each type of hole *)

let gen_acandi : model BatSet.t -> aexp BatSet.t
= fun models -> 
  BatSet.fold (fun (x,y,z) acc -> BatSet.add (Linear (x,"N",y,"i",z)) acc) models BatSet.empty 

let gen_bcandi : unit -> bexp BatSet.t
= fun () ->
  BatSet.add EqIOne
   (BatSet.add EqIN
    (BatSet.add (EqJ (ahole ()))
     (BatSet.add (Or (bhole (), bhole ())) BatSet.empty)))

let gen_pcandi : unit -> print BatSet.t  
= fun () ->
  BatSet.add (Print Star)
   (BatSet.add (Print Blank)
    (BatSet.add (If (bhole (), Star, Blank))
     (BatSet.add (If (bhole (), Blank, Star)) BatSet.empty)))

let gen_ccandi : unit -> col BatSet.t
= fun () -> 
  BatSet.add (For ("j", ahole (), phole ()))
   (BatSet.add (ColSeq (chole (), chole ())) BatSet.empty)

(* Replace each type of hole with a given candidate, 
   i.e., onestep transition *)

let replace_ah : state -> aexp -> aexp -> state
= fun (i,n,c) ah acandi ->
  let rec replace_ah_froma : aexp -> aexp -> aexp -> aexp
  = fun aexp ah acandi ->
    match aexp with
    | AHole _ when aexp=ah -> acandi
    | _ -> aexp in 
  let rec replace_ah_fromb : bexp -> aexp -> aexp -> bexp
  = fun bexp ah acandi ->
    match bexp with
    | EqJ a -> EqJ (replace_ah_froma a ah acandi)
    | Or (b1,b2) -> Or (replace_ah_fromb b1 ah acandi, replace_ah_fromb b2 ah acandi)
    | _ -> bexp in
  let rec replace_ah_fromp : print -> aexp -> aexp -> print
  = fun print ah acandi ->
    match print with
    | If (b,s1,s2) -> If (replace_ah_fromb b ah acandi, s1, s2) 
    | _ -> print in
  let rec replace_ah_fromc : col -> aexp -> aexp -> col
  = fun col ah acandi ->
    match col with
    | For (j,a,p) -> For (j, replace_ah_froma a ah acandi, replace_ah_fromp p ah acandi)
    | ColSeq (c1,c2) -> ColSeq (replace_ah_fromc c1 ah acandi, replace_ah_fromc c2 ah acandi)
    | _ -> col
  in (i, n, replace_ah_fromc c ah acandi)

let replace_bh : state -> bexp -> bexp -> state 
= fun (i,n,c) bh bcandi ->
  let rec replace_bh_fromb : bexp -> bexp -> bexp -> bexp
  = fun bexp bh bcandi ->
    match bexp with
    | Or (b1,b2) -> Or (replace_bh_fromb b1 bh bcandi, replace_bh_fromb b2 bh bcandi)
    | BHole id when bexp=bh -> bcandi 
    | _ -> bexp in
  let rec replace_bh_fromp : print -> bexp -> bexp -> print
  = fun print bh bcandi ->
    match print with
    | If (b,s1,s2) -> If (replace_bh_fromb b bh bcandi, s1, s2)
    | _ -> print in
  let rec replace_bh_fromc : col -> bexp -> bexp -> col
  = fun col bh bcandi ->
    match col with
    | For (j,a,p) -> For (j, a, replace_bh_fromp p bh bcandi) 
    | ColSeq (c1,c2) -> ColSeq (replace_bh_fromc c1 bh bcandi, replace_bh_fromc c2 bh bcandi)
    | _ -> col
  in (i, n, replace_bh_fromc c bh bcandi)

let replace_ph : state -> print -> print -> state
= fun (i,n,c) ph pcandi ->
  let rec replace_ph_fromp : print -> print -> print -> print
  = fun print ph pcandi ->
    match print with
    | PHole id when print=ph -> pcandi
    | _ -> print in
  let rec replace_ph_fromc : col -> print -> print -> col
  = fun col ph pcandi ->
    match col with
    | For (j,a,p) -> For (j, a, replace_ph_fromp p ph pcandi)
    | ColSeq (c1,c2) -> ColSeq (replace_ph_fromc c1 ph pcandi, replace_ph_fromc c2 ph pcandi)
    | _ -> col
  in (i, n, replace_ph_fromc c ph pcandi)  

let replace_ch : state -> col -> col -> state  
= fun (i,n,c) ch ccandi ->
  let rec replace_ch_fromc : col -> col -> col -> col
  = fun col ch ccandi ->
    match col with
    | ColSeq (c1,c2) -> ColSeq (replace_ch_fromc c1 ch ccandi, replace_ch_fromc c2 ch ccandi) 
    | CHole id when col=ch -> ccandi
    | _ -> col
  in (i, n, replace_ch_fromc c ch ccandi)

(* Collect all possible next states for each hole *)

let nextfor_ah : model BatSet.t -> state -> aexp -> state BatSet.t 
= fun models state ah ->
  let candidates = gen_acandi models in
    BatSet.fold (fun acandi acc -> 
      BatSet.add (replace_ah state ah acandi) acc
    ) candidates BatSet.empty

let nextfor_bh : state -> bexp -> state BatSet.t
= fun state bh ->
  let candidates = gen_bcandi () in
    BatSet.fold (fun bcandi acc ->
      BatSet.add (replace_bh state bh bcandi) acc  
    ) candidates BatSet.empty

let nextfor_ph : state -> print -> state BatSet.t
= fun state ph -> 
  let candidates = gen_pcandi () in
    BatSet.fold (fun pcandi acc ->
      BatSet.add (replace_ph state ph pcandi) acc  
    ) candidates BatSet.empty

let nextfor_ch : state -> col -> state BatSet.t 
= fun state ch ->
  let candidates = gen_ccandi () in
    BatSet.fold (fun ccandi acc ->
      BatSet.add (replace_ch state ch ccandi) acc  
    ) candidates BatSet.empty

let next : model BatSet.t -> state -> state BatSet.t 
= fun models state ->
  let nextstates_a = BatSet.fold (fun ah acc -> BatSet.union acc (nextfor_ah models state ah)) (aholes state) BatSet.empty in
  let nextstates_b = BatSet.fold (fun bh acc -> BatSet.union acc (nextfor_bh state bh)) (bholes state) BatSet.empty in
  let nextstates_s = BatSet.fold (fun sh acc -> BatSet.union acc (nextfor_ph state sh)) (pholes state)  BatSet.empty in 
  let nextstates_c = BatSet.fold (fun ch acc -> BatSet.union acc (nextfor_ch state ch)) (choles state) BatSet.empty in
    BatSet.union nextstates_a (BatSet.union nextstates_b (BatSet.union nextstates_s nextstates_c))

let is_closed : state -> bool
= fun state -> 
  BatSet.is_empty (aholes state) && BatSet.is_empty (bholes state) && 
  BatSet.is_empty (pholes state) && BatSet.is_empty (choles state)

let is_solution : int list -> example list -> state -> bool
= fun rownums examples state -> 
  List.for_all2 (fun rownum example ->
    try 
      Lang.run rownum state = example 
    with
      | _ -> false
  ) rownums examples

let transform_example : example -> example
= fun ex ->
  String.map (fun c -> 
    match c with
    | '*' -> 'S'
    | ' ' -> 'B'
    | '\n' -> 'N'
    | _ -> c) ex

let not_prune : example list -> int list -> int list list -> state -> bool
= fun examples rownums colnums state ->
  let exmeta = List.combine rownums colnums in
  let not_pruned = 
    List.for_all2 (fun example (rownum,colnum) ->
      let overapprox = Str.regexp (Abs.run rownum colnum state) in
      let example = transform_example example in
        Str.string_match overapprox example 0 
    ) examples exmeta in
  let _ = if !Options.profile2 && not not_pruned then print_pruned_states rownums colnums state in
    not_pruned

let rec work : example list -> int list -> int list list -> model BatSet.t -> Workset.t -> pgm option
= fun examples rownums colnums models workset ->
  iter := ! iter + 1;
  (*if !iter mod 10000 = 0 && not (!Options.exp)  *)
  if !iter mod 50000 = 0 
  then 
    begin 
      print_string ("Iter : " ^ (string_of_int !iter) ^ " ");
      print_endline ((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time () -. !start_time))))
    end;
  if Sys.time () -. !start_time > 3600.0 then None
  else
  match Workset.choose workset with
  | None -> None
  | Some (state, remainingset) ->
    if is_closed state then 
      if is_solution rownums examples state then Some state 
      else work examples rownums colnums models remainingset
    else
      let _ = if !Options.profile0 then print_endline (to_string state) in
      let _ = if !Options.profile1 then print_not_pruned_states rownums colnums state in 
      let nextstates = next models state in
      let nextstates = 
        if !Options.enum then nextstates 
        else BatSet.map Baseopt.reorder (BatSet.filter Baseopt.not_deadcode_state nextstates) in
      let nextstates = 
        if !Options.enum || !Options.base then nextstates
        else BatSet.filter (not_prune examples rownums colnums) nextstates in (* collect only states not being pruned *)
      let updatedset = BatSet.fold Workset.add nextstates remainingset in 
        work examples rownums colnums models updatedset 

let synthesize : example list -> int list -> int list list -> state option
= fun examples rownums colnums ->
  let models = Solver.run rownums colnums in
  let _ = 
    if !Options.profile0 || !Options.profile1 || !Options.profile2 then
      let _ = 
        BatSet.iter (fun (a,b,c) -> 
          print_endline (string_of_int a ^ " " ^ string_of_int b ^ " " ^ string_of_int c)
        ) models in
      let _ = print_endline "" in () in
  let initial = ("i", "N", chole ()) in
  let workset = Workset.add initial Workset.empty in
    work examples rownums colnums models workset
