(** Terms *)
type expr = Const
          | Var of string
          | Abst of string * expr
	  | App of expr * expr
(** Print the term *)
val print_expr : expr -> unit
(** List of free variables in the term *)
val free_vars : expr -> string list
(** Closed form of the term *)
val close : expr -> expr
