open Expr

(* Type *)
type ty = Iota | Fun of ty * ty

(* Type Scheme *)
type ty' = Iota' | Fun' of ty' * ty' | TyVar' of int
let rec concretize : ty' -> ty = function
  | Iota' -> Iota
  | Fun'(t1, t2) -> Fun((concretize t1), (concretize t2))
  | TyVar' i -> Iota
let rec contains (t1 : ty') (t2 : ty') : bool =
  match t2 with
  | Iota' -> false
  | Fun'(t21, t22) -> (contains t1 t21) || (contains t1 t22)
  | TyVar' i -> t1 = t2
let rec print_ty' = function
  | Iota' -> print_string "-"
  | Fun'(t1, t2) ->
    print_string "(";
    print_ty' t1;
    print_string ",";
    print_ty' t2;
    print_string ")"
  | TyVar' i -> print_string ("a" ^ (string_of_int i))
(* Type inference failiure *)
exception TypeInferenceError

(* Type Env *)
type tyenv = string -> ty'
let empty_env : tyenv = fun x -> raise TypeInferenceError
let add_env (te : tyenv) (x : string) (t : ty') : tyenv =
  fun id -> if id = x then t else (te id)

(* Subst *)
type subst = ty' -> ty'
let empty_subst : subst = fun t -> t
let new_subst (a : ty') (t' : ty') : subst =
  fun t ->
    let rec substitute (t : ty') : ty' =
      match t with
      | Iota' -> Iota'
      | Fun'(t1, t2) -> Fun'((substitute t1), (substitute t2))
      | TyVar' i -> 
        if t = a then t' else t
    in
    substitute t
let compose (s2 : subst) (s1 : subst) : subst =
  fun t -> s2 (s1 t)
let rec subst_env (s : subst) (te : tyenv) : tyenv =
  fun x -> s (te x)

(* Unify *)
let rec unify ((t1 : ty'), (t2 : ty')) : subst =
  if t1 = t2
  then empty_subst
  else
    match t1, t2 with
    | TyVar' i, _ ->
      if (contains t1 t2) then raise TypeInferenceError
      else new_subst t1 t2
    | _, TyVar' i ->
      if (contains t2 t1) then raise TypeInferenceError
      else new_subst t2 t1
    | Fun'(t11, t12), Fun'(t21, t22) ->
      let s1 = unify (t11, t21) in
      let s2 = unify ((s1 t12), (s1 t22)) in
      compose s1 s2
    | _ -> raise TypeInferenceError
