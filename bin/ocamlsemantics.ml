(*
  Normalization‑by‑Evaluation (NbE) in OCaml
  ------------------------------------------
  Variant  : Using actual lambda expressions in OCaml for semantics (see Wikipedia: https://en.wikipedia.org/wiki/Normalisation_by_evaluation#Outline)
*)

(* --- Syntax ------------------------------------------------------------- *)

type ty = 
  | Base of int 
  | Arrow of ty * ty 

type expr =
  | Var  of string
  | Lam  of string * expr
  | App  of expr * expr

(* Pretty‑printer (α‑renamed variables print nicely) *)
let rec pp = function
  | Var  x        -> x
  | Lam (x,e)     -> Printf.sprintf "(λ%s. %s)" x (pp e)
  | App (e1,e2)   -> Printf.sprintf "(%s %s)" (pp e1) (pp e2)

(* --- Semantic domain ---------------------------------------------------- *)

type sem = 
  | LAM of (sem -> sem)
  | SYN of (expr)

type env = (string * sem) list

(* Lookup in the environment *)
let rec lookup x = function
  | (y,v)::rest -> if x = y then v else lookup x rest

(* --- Evaluation --------------------------------------------------------- *)

let rec eval (e : env) = function
  | Var  x        -> lookup x e
  | Lam (x,body)  -> 
      let lambda (v : sem) = (eval ((x, v) :: e) body) in 
      LAM lambda
  | App (e1,e2) -> 
      match (eval e e1) with 
        | LAM l -> l (eval e e2)

(* --- Read‑back (reify) -------------------------------------------------- *)

let counter = ref 0
let fresh () = incr counter; "x" ^ string_of_int !counter


let rec reflect (e : expr) = function
  | Arrow (a, b) -> 
    let lambda s = reflect (App (e, reify a s)) b in
    LAM lambda
  | Base _ -> SYN e

and reify (t : ty) (s : sem) = 
  begin match (t, s) with 
    | (Arrow (a, b), LAM l) ->
        let x = fresh () in 
        Lam (x, reify b (l (reflect (Var x) a)))
    | (Base _, SYN t) -> t
    end

        

(* Convenience: normalise an expression in the empty environment *)
let normalize ty e = reify ty (eval [] e)

(* --- Demo ----------------------------------------------------------------*)

(* Church numerals, just to show the machinery works                     *)
let church_zero =
  Lam ("s",
    Lam ("z", Var "z"))

let church_succ =
  Lam ("n",
    Lam ("s",
      Lam ("z",
        App (Var "s",
             App (App (Var "n", Var "s"), Var "z")))))

let k = Lam ("x", Lam ("y", Var "x"))
let s = Lam ("x", Lam ("y", Lam ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let skk = App (App (s, k), k)

let runming_example = Lam ("x", App (Lam ("y", Lam ("x", Var "y")), Var "x"))

let runming_example_no_lam = App (Lam ("y", Lam ("x", Var "y")), Var "x")

let eta_example = Lam ("f", Var "f")

let eta_example_no_lam = Lam ("x", App (Var "f", Var "x"))


let church_two =
  App (church_succ, App (church_succ, church_zero))




let () =
  (* let n = normalize (Arrow (Arrow (Base 0, Base 0), Arrow (Base 0, Base 0))) skk in *)
  (* let n = normalize (Arrow (Base 0, Arrow (Base 1, Base 0))) runming_example in *)
  (* let n = normalize (Arrow (Base 0, Arrow (Base 1, Base 0))) runming_example in *)
  (* let n = normalize (Arrow (Base 1, Base 0)) runming_example_no_lam in *)
  let n = normalize (Arrow (Arrow (Base 0, Base 1), Arrow (Base 0, Base 1))) eta_example in
  Printf.printf "Original:\n  %s\n%!" (pp eta_example);
  Printf.printf "β‑normal form:\n  %s\n%!" (pp n)
