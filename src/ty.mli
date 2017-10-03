(** Type *)
type ty = Iota | Fun of ty * ty

(** Type Scheme *)
type ty' = Iota' | Fun' of ty' * ty' | TyVar' of int
(** Turns type scheme into type *)
val concretize : ty' -> ty
(** Check whether first is used in the second *)
val contains : ty' -> ty' -> bool
(** Print type scheme *)
val print_ty' : ty' -> unit
(** Type inference failiure *)
exception TypeInferenceError

(** Type Env *)
type tyenv = string -> ty'
(** Empty type env *)
val empty_env : tyenv
(** Add new type scheme *)
val add_env : tyenv -> string -> ty' -> tyenv

(** Subst *)
type subst = ty' -> ty'
(** Empty subst *)
val empty_subst : subst
(** New subst with single subsitution *)
val new_subst : ty' -> ty' -> subst
(** Compose two substs *)
val compose : subst -> subst -> subst
(** Substitution on Type Env *)
val subst_env : subst -> tyenv -> tyenv

(** Unification *)
val unify : ty' * ty' -> subst
