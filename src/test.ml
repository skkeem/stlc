open OUnit

open Expr
open Ty

let test_w _ =
  (* 1 *)
  assert_equal ~msg:"0" Iota (W.infer (Const));
  (* x *)
  assert_equal ~msg:"1" (Fun(Iota, Iota)) (W.infer (Var "x"));
  (* \x.x *)
  assert_equal ~msg:"2" (Fun(Iota, Iota)) (W.infer (Abst("x", Var "x")));
  (* (\x.x) 1 *)
  assert_equal ~msg:"3" Iota (W.infer (App(Abst("x", Var "x"), Const)));
  (* (\x.x x) 1 *)
  assert_raises ~msg:"4" (TypeInferenceError) (fun x -> (W.infer (App(Abst("x", App(Var "x", Var "x")), Const))));
  (* (\x.x) ((\y.y) 1) *)
  assert_equal ~msg:"5" Iota (W.infer (App(Abst("x", Var "x"), App(Abst("y", Var "y"), Const))));
  (* (\x.x) (\y.y) *)
  assert_equal ~msg:"6" (Fun(Iota, Iota)) (W.infer (App(Abst("x", Var "x"), Abst("y", Var "y"))));
  (* x 1 *)
  assert_equal ~msg:"7" (Fun(Fun(Iota, Iota), Iota)) (W.infer (App(Var "x", Const)));
  (* (\x.(x x)) 1 *)
  assert_raises ~msg:"8" (TypeInferenceError) (fun x -> (W.infer (App(Abst("x", App(Var "x", Var "x")), Const))))

let test_m _ =
  (* 1 *)
  assert_equal ~msg:"0" Iota (M.infer (Const));
  (* x *)
  assert_equal ~msg:"1" (Fun(Iota, Iota)) (M.infer (Var "x"));
  (* \x.x *)
  assert_equal ~msg:"2" (Fun(Iota, Iota)) (M.infer (Abst("x", Var "x")));
  (* (\x.x) 1 *)
  assert_equal ~msg:"3" Iota (M.infer (App(Abst("x", Var "x"), Const)));
  (* (\x.x x) 1 *)
  assert_raises ~msg:"4" (TypeInferenceError) (fun x -> (M.infer (App(Abst("x", App(Var "x", Var "x")), Const))));
  (* (\x.x) ((\y.y) 1) *)
  assert_equal ~msg:"5" Iota (M.infer (App(Abst("x", Var "x"), App(Abst("y", Var "y"), Const))));
  (* (\x.x) (\y.y) *)
  assert_equal ~msg:"6" (Fun(Iota, Iota)) (M.infer (App(Abst("x", Var "x"), Abst("y", Var "y"))));
  (* x 1 *)
  assert_equal ~msg:"7" (Fun(Fun(Iota, Iota), Iota)) (M.infer (App(Var "x", Const)));
  (* (\x.(x x)) 1 *)
  assert_raises ~msg:"8" (TypeInferenceError) (fun x -> (M.infer (App(Abst("x", App(Var "x", Var "x")), Const))))

let suite =
  "STLC" >::: ["w" >:: test_w; "m" >:: test_m]

let _ =
  run_test_tt_main suite
