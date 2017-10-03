open Expr
open Ty

(* W algorithm *)
let infer e = 
  let var_cnt = ref 0 in
  let new_var () = var_cnt := (!var_cnt + 1); TyVar' !var_cnt in
  let rec w ((te : tyenv), (e : expr)) : ty' * subst = 
    match e with
    | Const ->
      (Iota', empty_subst)
    | Var x ->
      ((te x), empty_subst)
    | Abst (x, e') ->
      let a = new_var() in
      let t, s = w ((add_env te x a), e') in
      (Fun'((s a), t), s)
    | App (e1, e2) ->
      let t1, s1 = w (te, e1) in
      let t2, s2 = w ((subst_env s1 te), e2) in
      let a = new_var() in
      let s3 = unify (Fun'(t2, a), (s2 t1)) in
      ((s3 a), (compose s3 (compose s2 s1)))
  in
  let t, s = w (empty_env, (close e)) in
  concretize (s t)
