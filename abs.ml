open Lang

type absint =
  | ITop
  | Int of int  

type absbool =
  | BTop
  | True
  | False

type regex = string
and absval = regex (* a (strict) subset of {*,_,\n}^* *)  

module Memory = struct
  type t = (var,absint) BatMap.t
  let add = BatMap.add
  let mem = BatMap.mem
  let find x m = try BatMap.find x m with | _ -> raise (Failure ("Not found : " ^ x))
  let empty = BatMap.empty
end

let absint2int : absint -> int
= fun absint -> 
  match absint with
  | ITop -> raise (Failure "absint2int : unchangeable") (* never happen *)
  | Int n -> n 

let rec abseval_aexp : aexp -> Memory.t -> absint
= fun aexp mem ->
  match aexp with
  | Linear (x,n,y,i,z) -> Int (x * (absint2int (Memory.find n mem)) + y * (absint2int (Memory.find i mem)) + z)
  | AHole _ -> ITop

let abseq : absint -> absint -> absbool
= fun abs1 abs2 ->
  match abs1,abs2 with
  | Int n1,Int n2 -> if n1=n2 then True else False  
  | _ -> BTop

let absor : absbool -> absbool -> absbool 
= fun abs1 abs2 ->
  match abs1,abs2 with
  | True,_ 
  | _,True -> True
  | False,False -> False
  | _ -> BTop

let rec abseval_bexp : bexp -> Memory.t -> absbool 
= fun bexp mem ->
  match bexp with
  | EqIOne -> abseq (Memory.find "i" mem) (Int 1)
  | EqIN -> abseq (Memory.find "i" mem) (Memory.find "N" mem) 
  | EqJ a -> abseq (Memory.find "j" mem) (abseval_aexp a mem)
  | Or (b1,b2) -> absor (abseval_bexp b1 mem) (abseval_bexp b2 mem)
  | BHole _ -> BTop

let symbol2absval : symbol -> absval
= fun s ->
  match s with
  | Star -> "S"
  | Blank -> "B" 

let rec abseval_print : print -> Memory.t -> absval
= fun p mem ->
  match p with
  | Print s -> "\\(" ^ symbol2absval s ^ "\\)" 
  | If (b,s1,s2) ->
    (match abseval_bexp b mem with
     | BTop -> "\\(" ^ symbol2absval s1 ^ "\\|" ^ symbol2absval s2 ^ "\\)"
     | True -> "\\(" ^ symbol2absval s1 ^ "\\)"
     | False -> "\\(" ^ symbol2absval s2 ^ "\\)") 
  | PHole _ -> "\\(" ^ "S" ^ "\\|"  ^ "B" ^ "\\)"

let rec abseval_col : int list -> col -> Memory.t -> absval
= fun colnum col mem ->
  match col with
  | For (j,a,p) ->
    (match abseval_aexp a mem with
     | ITop ->  "\\(" ^ "\\(" ^ abseval_print p (Memory.add j ITop mem) ^ "\\)" ^ "*" ^ "\\)"  (* original implementation *)
       (* find-grained pruning, but it has performance problem *)
       (* let rec iter cur limit = 
         if cur <=limit then "\\(" ^ abseval_print p (Memory.add j (Int cur) mem) ^ "?" ^ "\\)" ^ iter (cur+1) limit 
         else "" in 
       iter 1 (List.nth colnum ((absint2int (Memory.find "i" mem)) - 1)) (* find the column number of i-th row *) *)
     | Int n -> 
       let rec iter cur limit =
         if cur <= limit then "\\(" ^ abseval_print p (Memory.add j (Int cur) mem) ^ "\\)" ^ iter (cur+1) limit
         else "" in
       iter 1 (absint2int (abseval_aexp a mem)))
  | ColSeq (c1,c2) -> "\\(" ^ abseval_col colnum c1 mem ^ abseval_col colnum c2 mem ^ "\\)"
  | CHole _ -> "\\(" ^ "\\(" ^ "S" ^ "\\|" ^ "B" ^ "\\)" ^ "*" ^ "\\)" 

let rec abseval_row : int list -> row -> Memory.t -> absval
= fun colnum (i,n,c) mem ->
  let rec iter cur limit = 
    if cur <= limit then "\\(" ^ abseval_col colnum c (Memory.add i (Int cur) mem) ^ "N" ^ "\\)" ^ iter (cur+1) limit (* N denotes for new line *) 
    else "" in
  iter 1 (absint2int (Memory.find n mem))

let run : int -> int list -> state -> absval
= fun rownum colnum state -> abseval_row colnum state (Memory.add "N" (Int rownum) Memory.empty)

(************************)
(* for pretty profiling *)
(************************)

let rec pabseval_print : print -> Memory.t -> absval
= fun p mem ->
  match p with
  | Print s -> (* "(" ^ *) symbol2absval s (* ^ ")" *) 
  | If (b,s1,s2) ->
    (match abseval_bexp b mem with
     | BTop -> "(" ^ symbol2absval s1 ^ "|" ^ symbol2absval s2 ^ ")"
     | True -> (* "(" ^ *) symbol2absval s1 (* ^ ")" *)
     | False -> (* "(" ^ *) symbol2absval s2 (* ^ ")" *)) 
  | PHole _ -> "(" ^ "S" ^ "|"  ^ "B" ^ ")"

let rec pabseval_col : int list -> col -> Memory.t -> absval
= fun colnum col mem ->
  match col with
  | For (j,a,p) ->
    (match abseval_aexp a mem with
     | ITop -> "(" ^ pabseval_print p (Memory.add j ITop mem) ^ ")" ^ "*" 
     (*  let rec iter cur limit =
         if cur <= limit then pabseval_print p (Memory.add j (Int cur) mem) ^ "?" ^ iter (cur+1) limit
         else "" in
       iter 1 (List.nth colnum ((absint2int (Memory.find "i" mem)) - 1)) *) 
     | Int n -> 
       let rec iter cur limit =
         if cur <= limit then (* "(" ^ *) pabseval_print p (Memory.add j (Int cur) mem) (* ^ ")" *) ^ iter (cur+1) limit
         else "" in
       iter 1 (absint2int (abseval_aexp a mem)))
  | ColSeq (c1,c2) -> (* "(" ^ *) pabseval_col colnum c1 mem ^ pabseval_col colnum c2 mem (* ^ ")" *)
  | CHole _ -> (* "(" ^ *) "(" ^ "S" ^ "|" ^ "B" ^ ")" ^ "*" (* ^ ")" *) 

let rec pabseval_row : int list -> row -> Memory.t -> absval
= fun colnum (i,n,c) mem ->
  let rec iter cur limit = 
    if cur <= limit then "{" ^ pabseval_col colnum c (Memory.add i (Int cur) mem) ^ "N" ^ "}  " ^ iter (cur+1) limit  
    else "" in
  iter 1 (absint2int (Memory.find n mem))

let pretty_run : int -> int list -> state -> absval
= fun rownum colnum state -> pabseval_row colnum state (Memory.add "N" (Int rownum) Memory.empty)
