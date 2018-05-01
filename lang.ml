type var = string

type example = string

type symbol = 
  | Star
  | Blank

type aexp =
  | Linear of int * var * int * var * int (* x*N + y*i + z *)
  | AHole of int 

type bexp =
  | EqIOne
  | EqIN
  | EqJ of aexp
  | Or of bexp * bexp
  | BHole of int

type print = 
  | Print of symbol 
  | If of bexp * symbol * symbol
  | PHole of int

type col =
  | For of var * aexp * print (* (j,a,p) *)
  | ColSeq of col * col
  | CHole of int

type row = var * var * col (* (i,N,c) *) 
and pgm = row
and state = pgm

let acnt = ref 0
let ahole : unit -> aexp
= fun () -> acnt:=!acnt+1; AHole (!acnt)

let bcnt = ref 0
let bhole : unit -> bexp
= fun () -> bcnt:=!bcnt+1; BHole (!bcnt)

let pcnt = ref 0
let phole : unit -> print
= fun () -> pcnt:=!pcnt+1; PHole (!pcnt)

let ccnt = ref 0
let chole : unit -> col
= fun () -> ccnt:=!ccnt+1; CHole (!ccnt)

(* Key : cost adjusments between if, or and colseq *)

let rec cost_a : aexp -> int
= fun aexp -> 
  match aexp with
  | Linear _ -> 15
  | AHole _ -> 90

let rec cost_b : bexp -> int
= fun bexp ->
  match bexp with
  | EqIOne -> 15
  | EqIN -> 15
  | EqJ a -> 15 + cost_a a
  | Or (b1,b2) -> 5 + cost_b b1 + cost_b b2
  | BHole _ -> 95

let rec cost_p : print -> int
= fun print ->
  match print with
  | Print _ -> 5
  | If (b,l1,l2) -> 65 + cost_b b 
  | PHole _ -> 90

let rec cost_c : col -> int
= fun col ->
  match col with
  | For (_,a,p) -> 15 + cost_a a + cost_p p 
  | ColSeq (c1,c2) -> 5 + cost_c c1 + cost_c c2
  | CHole _ -> 100

let cost_row : row -> int
= fun (_,_,c) -> cost_c c

let cost : pgm -> int
= fun pgm -> cost_row pgm

let iterator = "i"
let rownum = "N"

let star = "*" 
let blank = " "
let linebreak = "\n"

let ts_aexp : aexp -> string
= fun aexp ->
  match aexp with
  (* | Linear (x,n,y,i,z) -> "x*N + y*i + z" *)
  | Linear (x,n,y,i,z) -> string_of_int x ^ "*N" ^ " + " ^ string_of_int y ^ "*i" ^ " + " ^ string_of_int z 
  | AHole _ -> "AH"

let rec ts_bexp : bexp -> string
= fun bexp ->
  match bexp with
  | EqIOne -> "i == 1"
  | EqIN -> "i == N" 
  | EqJ a -> "j == " ^ ts_aexp a
  | Or (b1,b2) -> ts_bexp b1 ^ " || " ^ ts_bexp b2
  | BHole _ -> "BH" 

let ts_symbol : symbol -> string
= fun s ->
  match s with
  | Star -> "*"
  | Blank -> "_"

let rec ts_print : print -> string
= fun print ->
  match print with
  | Print s -> "print " ^ ts_symbol s 
  | If (b,s1,s2) -> "if " ^ "(" ^ ts_bexp b ^ ") " ^ ts_symbol s1 ^ " else " ^ ts_symbol s2
  | PHole _ -> "PH"

let rec ts_col : col -> string
= fun col ->
  match col with
  | For (j,a,l) -> "for " ^ j ^ " in range " ^ ts_aexp a ^ " do {" ^ ts_print l ^ "}"
  | ColSeq (c1,c2) -> ts_col c1 ^ "; " ^ ts_col c2  
  | CHole _ -> "CH"

let ts_row : row -> string
= fun (i,n,c) -> "for " ^ i ^ " in range " ^ n ^ " do {" ^ ts_col c ^ "; print '\\n'}"

let to_string : pgm -> string
= fun pgm -> ts_row pgm

(* pretty print *)

let line = ref 1

let pretty_aexp : aexp -> string
= fun aexp ->
  match aexp with
  (* | Linear (x,n,y,i,z) -> "x*N + y*i + z" *)
  | Linear (x,n,y,i,z) -> 
    let str = 
      if x = 0 then "" else if x = 1 then "N" else 
      if x = -1 then "-" ^ "N" 
      else string_of_int x ^ "*N" in
    let str = 
      if y = 0 then str else 
      if y = 1 then 
        if str = "" then "i"
        else str ^ " + " ^ "i" else 
      if y = -1 then 
        if str = "" then " - " ^ "i"
        else str ^ " - " ^ "i" 
      else 
        if str = "" then string_of_int y ^ "*i" 
        else str ^ " + " ^ string_of_int y ^ "*i" in
    let str = 
      if z = 0 then str else
      if z < 0 then 
        if str = "" then " - " ^ string_of_int (z*(-1)) 
        else str ^ " - " ^ string_of_int (z*(-1))
      else 
        if str = "" then string_of_int z
        else str ^ " + " ^ string_of_int z
    in str
  | AHole _ -> "AH"

let rec pretty_bexp : bexp -> string
= fun bexp ->
  match bexp with
  | EqIOne -> "i == 1"
  | EqIN -> "i == N" 
  | EqJ a -> "j == " ^ pretty_aexp a
  | Or (b1,b2) -> pretty_bexp b1 ^ " || " ^ pretty_bexp b2
  | BHole _ -> "BH" 

let pretty_symbol : symbol -> string
= fun s ->
  match s with
  | Star -> "*"
  | Blank -> " "

let rec pretty_print : print -> string
= fun print ->
  match print with
  | Print s -> "    " ^ "print " ^ "'" ^ pretty_symbol s ^ "'" 
  | If (b,s1,s2) ->
    "    " ^
    "if " ^ "(" ^ pretty_bexp b ^ ") " ^ "print " ^ "'" ^ pretty_symbol s1 ^ "'" ^ "\n" ^
    "    " ^
    "else " ^ "print " ^ "'" ^ pretty_symbol s2 ^ "'"
  | PHole _ -> "PH"

let rec pretty_col : col -> string
= fun col ->
  match col with
  | For (j,a,l) -> "  " ^ "for " ^ j ^ " in range " ^ pretty_aexp a ^ " do:\n" ^ pretty_print l ^ ";\n"
  | ColSeq (c1,c2) -> pretty_col c1 ^ pretty_col c2  
  | CHole _ -> "CH"

let pretty_row : row -> string
= fun (i,n,c) -> "for " ^ i ^ " in range " ^ n ^ " do:" ^ "\n" ^ pretty_col c ^ "  print '\\n';"

let pretty_pgm : pgm -> string
= fun pgm -> pretty_row pgm

(* pretty print for C *)

let c_pretty_aexp : aexp -> string
= fun aexp ->
  match aexp with
  (* | Linear (x,n,y,i,z) -> "x*N + y*i + z" *)
  | Linear (x,n,y,i,z) -> 
    let str = 
      if x = 0 then "" else if x = 1 then "n" else 
      if x = -1 then "-" ^ "n" 
      else string_of_int x ^ "*n" in
    let str = 
      if y = 0 then str else 
      if y = 1 then 
        if str = "" then "i"
        else str ^ " + " ^ "i" else 
      if y = -1 then 
        if str = "" then " - " ^ "i"
        else str ^ " - " ^ "i" 
      else 
        if str = "" then string_of_int y ^ "*i" 
        else str ^ " + " ^ string_of_int y ^ "*i" in
    let str = 
      if z = 0 then str else
      if z < 0 then 
        if str = "" then " - " ^ string_of_int (z*(-1)) 
        else str ^ " - " ^ string_of_int (z*(-1))
      else 
        if str = "" then string_of_int z
        else str ^ " + " ^ string_of_int z
    in str
  | AHole _ -> "AH"

let rec c_pretty_bexp : bexp -> string
= fun bexp ->
  match bexp with
  | EqIOne -> "i == 1"
  | EqIN -> "i == n" 
  | EqJ a -> "j == " ^ c_pretty_aexp a
  | Or (b1,b2) -> c_pretty_bexp b1 ^ " || " ^ c_pretty_bexp b2
  | BHole _ -> "BH" 

let c_pretty_symbol : symbol -> string
= fun s ->
  match s with
  | Star -> "*"
  | Blank -> " "

let rec c_pretty_print : print -> string
= fun print ->
  match print with
  | Print s -> "      " ^ "printf " ^ "(\"" ^ c_pretty_symbol s ^ "\");" 
  | If (b,s1,s2) ->
    (* 6 blanks *)
    "      " ^ "if " ^ "(" ^ c_pretty_bexp b ^ ")" ^ "\n" ^
    (* 8 blanks *) 
    "        " ^ "printf " ^ "(\"" ^ c_pretty_symbol s1 ^ "\");" ^ "\n" ^
    (* 6 blanks *)
    "      " ^ "else" ^ "\n" ^ 
    (* 8 blanks *)
    "        " ^ "printf " ^ "(\"" ^ c_pretty_symbol s2 ^ "\");"
  | PHole _ -> "PH"

let rec c_pretty_col : col -> string
= fun col ->
  match col with
  | For (j,a,l) -> 
    "    " ^ "for (j = 1; j <= " ^ c_pretty_aexp a ^ "; j++) {" ^ "\n" ^
    c_pretty_print l ^ "\n" ^
    "    " ^ "}" ^ "\n" 
  | ColSeq (c1,c2) -> c_pretty_col c1 ^ c_pretty_col c2  
  | CHole _ -> "CH"

let c_pretty_row : row -> string
= fun (i,n,c) -> 
  "  for (i = 1; i <= n; i++) {" ^ "\n" ^ 
  c_pretty_col c ^ 
  "    printf (\"\\n\");" ^ "\n" ^
  "  }"

let c_pretty_pgm : pgm -> string
= fun pgm -> 
  "#include <stdio.h>" ^ "\n\n" ^
  "int main() {" ^ "\n" ^
  "  " ^ "int n, i, j;" ^ "\n" ^
  "  " ^ "scanf (\"%d\", &n);" ^ "\n\n" ^
  c_pretty_row pgm ^ "\n\n" ^
  "  " ^ "return 0;" ^ "\n" ^
  "}"

module Memory = struct
  type t = (var,int) BatMap.t
  let add = BatMap.add
  let mem = BatMap.mem
  let find x m = try BatMap.find x m with | _ -> raise (Failure ("Not found : " ^ x))
  let empty = BatMap.empty
end

type value = string (* a (strict) subset of {*,_,\n} *)

let rec eval_aexp : aexp -> Memory.t -> int
= fun aexp mem ->
  match aexp with
  | Linear (x,n,y,i,z) -> x * (Memory.find n mem) + y * (Memory.find i mem) + z 
  | AHole _ -> raise (Failure "eval_aexp : hole encountered")

let rec eval_bexp : bexp -> Memory.t -> bool
= fun bexp mem ->
  match bexp with
  | EqIOne -> Memory.find "i" mem = 1
  | EqIN -> Memory.find "i" mem = Memory.find "N" mem  
  | EqJ a -> Memory.find "j" mem = eval_aexp a mem
  | Or (b1,b2) -> eval_bexp b1 mem || eval_bexp b2 mem
  | BHole _ -> raise (Failure "eval_bexp : hole encountered") 

let eval_symbol : symbol -> value
= fun s ->
  match s with
  | Star -> "*"
  | Blank -> " "

let rec eval_print : print -> Memory.t -> value
= fun p mem ->
  match p with
  | Print s -> eval_symbol s 
  | If (b,s1,s2) -> if eval_bexp b mem then eval_symbol s1 else eval_symbol s2 
  | PHole _ -> raise (Failure "eval_print : hole encountered") 

let rec eval_col : col -> Memory.t -> value
= fun col mem ->
  match col with
  | For (j,a,p) ->
    let rec iter cur limit =
      if cur <= limit then eval_print p (Memory.add j cur mem) ^ iter (cur+1) limit
      else "" in
    iter 1 (eval_aexp a mem)
  | ColSeq (c1,c2) -> eval_col c1 mem ^ eval_col c2 mem    
  | CHole _ -> raise (Failure "eval_col : hole encountered")

let rec eval_row : row -> Memory.t -> value
= fun (i,n,c) mem ->
  let rec iter cur limit = 
    if cur <= limit then eval_col c (Memory.add i cur mem) ^ linebreak ^ iter (cur+1) limit  
    else "" in
  iter 1 (Memory.find n mem)
    
let run : int -> pgm -> string
= fun rownum pgm -> eval_row pgm (Memory.add "N" rownum Memory.empty)
