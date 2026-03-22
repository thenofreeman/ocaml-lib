(* vector.mli *)

type t
(* type u_reduction = t -> float *)
(* type u_transform = t -> ?ip -> t *)
(* type b_reduction = t -> float *)
(* type b_transform = t -> ?ip -> t -> t *)

val from : float array -> t
val create : int -> t
val make : int -> float -> t
val init : int -> (int -> float) -> t
val zero : int -> t
val empty : t
val random : ?range:float*float -> int -> t

val get : t -> int -> float
val set : t -> int -> t

val dim : t -> int
val is_empty : t -> bool

val linspace : float -> float -> int -> t -> t
val logspace : float -> float -> int -> t -> t

val of_array : float array -> t
val to_array : t -> float list

val of_list : float list -> t
val to_list : t -> float list

val same : ?eps:float -> t -> t -> bool

val append : t -> t -> t
val concat : t list -> t

val map : (float -> float) -> ?ip:bool -> t -> t
val map2 : (float -> float) -> t -> t -> t
val iter : (float -> unit) -> t -> unit
val iter2 : (float -> unit) -> t -> t -> unit
val iteri : (int -> float -> unit) -> t -> unit
val iter2i : (int -> float -> unit) -> t -> t -> unit
val fold : ('a -> float -> 'a) -> 'a -> t -> 'a
val fold2 : ('a -> float -> 'a) -> 'a -> t -> t -> 'a

val rev : t -> t
val sort : ?cmp:(float -> float -> int) -> ?ip:bool -> t -> t

val arg_min : t -> int
val arg_max : t -> int

(* unary reductions *)
val min : t -> float
val max : t -> float
val sum : t -> float
val prod : t -> float
val l1_norm : t -> float
val l2_norm : t -> float
val chebyshev_norm : t -> float
val minkowski_norm : t -> float

(* unary transforms *)
val recip : t -> ?ip:bool -> t
val scale : t -> ?ip:bool -> float -> t
val scalar_add: t -> ?ip:bool -> float -> t
val scalar_sub: t -> ?ip:bool -> float -> t
val neg : t -> ?ip:bool -> t
val abs : t -> ?ip:bool -> t
val sign : t -> ?ip:bool -> t
val unit_vec : t -> ?ip:bool -> t

(* binary reductions *)
val dot : t -> t -> float

(* binary transforms *)
val add : t -> ?ip:bool -> t -> t
val sub : t -> ?ip:bool -> t -> t
val mult : t -> ?ip:bool -> t -> t
val div : t -> ?ip:bool -> t -> t

(* io *)
val pp : t -> unit
