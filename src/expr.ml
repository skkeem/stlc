(* Terms *)
type expr = Const
          | Var of string
          | Abst of string * expr
	  | App of expr * expr

let rec print_expr = function
  | Const -> print_string "*"
  | Var x -> print_string x
  | Abst (x, e) ->
    print_string ("\\" ^ x ^ ".(");
    print_expr e;
    print_string ")"
  | App (e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string " ";
    print_expr e2;
    print_string ")"

let rec free_vars : expr -> string list = function
  | Const -> []
  | Var x -> [x]
  | Abst (x, e) ->
    let fv = (free_vars e) in
    fv
    |> List.filter (fun id -> id <> x)
  | App (e1, e2) ->
    let fv1 = (free_vars e1) in
    let fv2 = (free_vars e2) in
    fv2
    |> List.rev_append fv1
    |> List.sort_uniq compare

let close (e : expr) : expr =
  let fv = (free_vars e) in
  fv
  |> List.fold_left (fun e' fv' -> Abst (fv', e')) e
