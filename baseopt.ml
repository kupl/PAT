open Lang

(**************************************************)
(* 1. Removing deadcode state by SAT solving      *)
(**************************************************)

(* denote a dominating (x,y,z) of the inner loop *)

type formula = 
  | Unknown 
  | LoopBound of int * int * int

let rec transform_aexp : formula -> aexp -> aexp
= fun formula aexp -> aexp

(* Note : unsatisfiable if cond - loop_bound > 0 *)
let cond_exceed_loopbound : int * int * int -> int * int * int -> bool
= fun cond loopbound ->
  let (x,y,z) = cond in
  let (x',y',z') = loopbound in
    match x-x', y-y', z-z' with 
    | x'',0,0 when x''>0 -> true(* x''*N > 0 *) 
    | 0,y'',0 when y''>0 -> true (* y''*i > 0 *)
    | 0,0,z'' when z''>0 -> true (* z'' > 0 *)
    | x'',y'',0 when x''>0 && y''<0 && x''>(-1*y'') -> true (* e.g. 2*N + -1*i + 0 > 0 *)
    | x'',y'',z'' when x''>0 && y''<0 && x''>=(-1*y'') && z''>0 -> true (* e.g. 2*N + -2*i + 1 *)
    | _ -> false

let rec dead_bexp : formula -> bexp -> bool
= fun formula bexp ->
  match bexp with
  | EqJ (Linear (0,_,0,_,0)) -> true (* 1 <= j <= ... /\ j = 0 is always unsatisfiable, i.e. dead expression *)
  | EqJ (Linear (x,_,y,_,z)) ->
    (match formula with
     | LoopBound (x',y',z') when cond_exceed_loopbound (x,y,z) (x',y',z') -> true
     | _ -> false)
  | Or (b1,b2) -> dead_bexp formula b1 || dead_bexp formula b2 
  | _ -> false

let rec dead_print : formula -> print -> bool
= fun formula print ->
  match print with
  | If (b,_,_) -> dead_bexp formula b  
  | _ -> false

let rec dead_col : formula -> col -> bool
= fun formula col -> 
  match col with
  | For (_,Linear (0,_,0,_,0),_) -> true (* 1 <= j <= 0 is always unsatisfiable, i.e. dead code *)
  | For (_,Linear (x,_,y,_,z),p) -> dead_print (LoopBound (x,y,z)) p
  | For (_,AHole id,p) -> dead_print Unknown p (* Unknown means we do not know upper bound, i.e. 1 <= j <= ?? *)
  | ColSeq (c1,c2) -> dead_col formula c1 || dead_col formula c2 
  | _ -> false

let dead_row : formula -> row -> bool
= fun formula (i,n,c) -> dead_col formula c

let deadcode_state : state -> bool
= fun state -> dead_row Unknown state 

let not_deadcode_state : state -> bool
= fun state -> not (deadcode_state state)

(*********************************************)
(* 2. State normalization by                 *) 
(* reordering in the order of i -> j -> hole *)
(*********************************************)

(* returns 'i', 'j', and 'bhole' set, respectively *)

let grouping_bexp : bexp -> bexp BatSet.t * bexp BatSet.t * bexp BatSet.t
= fun bexp ->
  let rec grouping_bexp' b (iset,jset,hset) =
    match b with
    | EqIOne -> (BatSet.add EqIOne iset, jset, hset)
    | EqIN -> (BatSet.add EqIN iset, jset, hset)
    | EqJ a -> (iset, BatSet.add (EqJ a) jset, hset)
    | Or (b1,b2) ->
      let (iset',jset',hset') = grouping_bexp' b1 (iset,jset,hset) in
        grouping_bexp' b2 (iset', jset', hset')
    | BHole id -> (iset, jset, BatSet.add (BHole id) hset) 
  in grouping_bexp' bexp (BatSet.empty,BatSet.empty,BatSet.empty)

let bset2bexp : bexp BatSet.t -> bexp option
= fun bset ->
  let rec bset2bexp' bset bexp = 
    if BatSet.is_empty bset then Some bexp
    else 
      let (elem,remaining) = BatSet.pop bset in bset2bexp' remaining (Or (bexp,elem)) in
  (* end of bset2bexp' *)
  if BatSet.is_empty bset then None
  else if BatSet.cardinal bset = 1 then Some (BatSet.choose bset)
  else
    let (elem,remaining) = BatSet.pop bset in 
      bset2bexp' remaining elem  

let rec reorder_bexp : bexp -> bexp
= fun bexp -> 
  match bexp with
  | Or (b1,b2) ->
    let (iset,jset,hset) = grouping_bexp (Or (b1,b2)) in
    (match bset2bexp iset, bset2bexp jset, bset2bexp hset with
     | None,None,None -> raise (Failure "reorder_bexp") (* never happen *)
     | None,None,Some b
     | None,Some b,None 
     | Some b,None,None -> b
     | None,Some b1,Some b2 
     | Some b1,None,Some b2
     | Some b1,Some b2,None -> Or (b1,b2)
     | Some b1,Some b2,Some b3 -> Or (b1, Or (b2,b3)))
  | _ -> bexp

let reorder_print : print -> print
= fun print -> 
  match print with
  | If (b,s1,s2) -> If (reorder_bexp b, s1, s2)
  | _ -> print

let rec reorder_col : col -> col
= fun col ->
  match col with
  | ColSeq (c1,c2) -> ColSeq (reorder_col c1, reorder_col c2)
  | For (j,a,p) -> For (j, a, reorder_print p)
  | _ -> col 

let reorder_row : row -> row
= fun (i,n,c) -> (i, n, reorder_col c)

let reorder : state -> state
= fun state -> reorder_row state
