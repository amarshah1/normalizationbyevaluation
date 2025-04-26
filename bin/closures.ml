(*
  Normalization‑by‑Evaluation (NbE) in OCaml
  ------------------------------------------
  Variant  : Intensional residualizing semantics with read‑back
             (“Choice {1,1}” in W. J. Bowman's tutorial).
*)

(* --- Syntax ------------------------------------------------------------- *)

type expr =
  (* | Base of int                    constants – you can swap in any type *)
  | Var  of string
  | Lam  of string * expr
  | App  of expr * expr

(* Pretty‑printer (α‑renamed variables print nicely) *)
let rec pp = function
  (* | Base n        -> string_of_int n *)
  | Var  x        -> x
  | Lam (x,e)     -> Printf.sprintf "(λ%s. %s)" x (pp e)
  | App (e1,e2)   -> Printf.sprintf "(%s %s)" (pp e1) (pp e2)

(* --- Semantic domain ---------------------------------------------------- *)

(* Neutrals (stuck eliminations) *)
type neutral =
  | NVar of string
  | NApp of neutral * value        (* target is neutral, argument any value *)

and value =
  (* | VBase of int *)
  | VClosure of env * string * expr   (* λ‑closures carry their env *)
  | VNeutral of neutral

(* todo : write a print function for value*)


and env = (string * value) list

(* Lookup with fallback to neutral variable (residualising) *)
let rec lookup x = function
  (* | [] -> VNeutral (NVar x) *)
  | (y,v)::rest -> if x = y then v else lookup x rest

(* --- Evaluation --------------------------------------------------------- *)

let rec eval (e : env) = function
  | Var  x        -> lookup x e
  | Lam (x,body)  -> VClosure (e,x,body)
  | App (e1,e2)   -> do_app (eval e e1) (eval e e2)

and do_app v1 v2 =
  match v1 with
  | VClosure (e,x,body) -> eval ((x,v2)::e) body
  | VNeutral neu        -> VNeutral (NApp (neu,v2))

(* --- Read‑back (reify) -------------------------------------------------- *)

let counter = ref 0
let fresh () = incr counter; "x" ^ string_of_int !counter

let rec reify_value = function
  (* | VBase n          -> Base n *)
  | VNeutral neu     -> reify_neutral neu
  | VClosure _ as v  ->
      let x = fresh () in
      let vx = VNeutral (NVar x) in
      let body = reify_value (do_app v vx) in
      Lam (x, body)

and reify_neutral = function
  | NVar x        -> Var x
  | NApp (n,v)    -> App (reify_neutral n, reify_value v)

(* Convenience: normalise an expression in the empty environment *)
let normalize e = reify_value (eval [] e)

(* --- Demo ----------------------------------------------------------------*)

(* Church numerals*)
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

let eta_example = Lam ("f", Lam ("x", App (Var "f", Var "x")))

let church_two =
  App (church_succ, App (church_succ, church_zero))

let () =
  let n = normalize eta_example in
  Printf.printf "Original:\n  %s\n%!" (pp eta_example);
  Printf.printf "β‑normal form:\n  %s\n%!" (pp n)
