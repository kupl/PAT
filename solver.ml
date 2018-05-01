open Z3
open Z3.Solver
open Lang
open Helper

type model = int * int * int

type restraint = Expr.expr

let ctx = Z3.mk_context []
(* wrappers *)
let mk_var = Arithmetic.Integer.mk_const_s ctx 
let mk_int = Arithmetic.Integer.mk_numeral_i ctx  
let plus lst = Arithmetic.mk_add ctx lst  
let mult lst = Arithmetic.mk_mul ctx lst
let lt e1 e2 = Arithmetic.mk_lt ctx e1 e2
let le e1 e2 = Arithmetic.mk_le ctx e1 e2
let eq e1 e2 = Boolean.mk_eq ctx e1 e2
let neq e1 e2 = Boolean.mk_not ctx (eq e1 e2)
let or' lst = Boolean.mk_or ctx lst 
let exp2int e = Arithmetic.Integer.get_int e

(* returns unit *)
let add solver lst = Z3.Solver.add solver lst 
let solver = Z3.Solver.mk_solver ctx None
let x = mk_var "x" 
let y = mk_var "y" 
let z = mk_var "z" 
let check solver lst = Z3.Solver.check solver lst

let model_of model (x,y,z) = 
  (Z3.Model.get_const_interp_e model x,
   Z3.Model.get_const_interp_e model y,
   Z3.Model.get_const_interp_e model z)

(* Each the iteration number of each loop is bounded by 
   the number of columns at each row. *)
let first : int -> int list -> restraint list
= fun rownum colnum ->
  let (_, constraints) = 
    List.fold_left (fun (ith,acc) col -> 
      let left = [le (mk_int 0) (plus [mult [mk_int rownum; x]; mult [mk_int ith; y]; z])] in
      let right = [le (plus [mult [mk_int rownum; x]; mult [mk_int ith; y]; z]) (mk_int col)] in
        (ith+1, acc@left@right)) (1,[]) colnum in
    constraints

(* We guide the absolute value of z to be less than the number of rows (n_r) *)
(* For example, suppose n_r = 3 and z = 7 are given, then
   we can represent z as 2*n_r + 1 without loss of generality. *)
let second : int -> restraint list
= fun rownum -> [lt (mk_int (-rownum)) z] @ [lt z (mk_int rownum)]

let gen_constraint : int -> int list -> restraint list
= fun rownum colnum -> (first rownum colnum) @ (second rownum) 

let gen_constraints : int list -> int list list -> restraint list
= fun rownums colnums -> 
  List.fold_left2 (fun acc r c -> acc@(gen_constraint r c)) [] rownums colnums

let rec compute_models () =
  match check solver [] with
  | UNSATISFIABLE -> BatSet.empty
  | UNKNOWN -> raise (Failure "Z3 UNKNOWN") 
  | SATISFIABLE -> 
    (match Z3.Solver.get_model solver with 
     | Some model -> 
       (match model_of model (x,y,z) with
        | Some x', Some y', Some z' ->
          let xint = exp2int x' in
          let yint = exp2int y' in
          let zint = exp2int z' in
          (* let _ = print_endline (string_of_int xint ^ " " ^ string_of_int yint ^ " " ^ string_of_int zint) in *) 
          let _ = add solver [or' [neq x x'; neq y y'; neq z z']] in
            BatSet.add (xint,yint,zint) (compute_models ())
        | _ -> raise (Failure "never happen"))
     | _ -> raise (Failure "never happen"))
  
let run : int list -> int list list -> model BatSet.t 
= fun rownums colnums ->
  let constraints = gen_constraints rownums colnums in
  let _ = add solver constraints in (* add constraints to solver *) 
    compute_models () 
