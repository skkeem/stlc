open Expr
open Ty

(** M algorithm *)
let infer e = 
  let var_cnt = ref 0 in
  let new_var () = var_cnt := (!var_cnt + 1); TyVar' !var_cnt in
  let rec m ((te : tyenv), (e : expr), (t : ty')) : subst = 
    match e with
    | Const ->
      unify (t, Iota')
    | Var x ->
      unify (t, (te x))
    | Abst (x, e') ->
      let a1 = new_var() in
      let a2 = new_var() in
      let s1 = unify (Fun'(a1, a2), t) in
      let te' = subst_env s1 te in
      let a1' = s1 a1 in
      let a2' = s1 a2 in
      let s2 = m ((add_env te' x a1'), e', a2') in
      compose s2 s1
    | App (e1, e2) ->
      let a = new_var() in
      let s1 = m (te, e1, Fun'(a, t)) in
      let s2 = m ((subst_env s1 te), e2, (s1 a)) in
      compose s2 s1
  in
  let a = new_var() in
  let s = m (empty_env, (close e), a) in
  concretize (s a)
