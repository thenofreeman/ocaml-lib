(* vector.mli *)

type t = Lacaml.D.vec
(* type b_transform = t -> ?ip -> t -> t *)
(* type b_reduction = t -> float *)
(* type u_transform = t -> ?ip -> t *)
(* type u_reduction = t -> float *)

val create : int -> t
val make : int -> float -> t
val init : int -> (int -> float) -> t
val zero : int -> t
val empty : t
val random : ?range:float*float -> int -> t

val get : t -> int -> float
val set : t -> int -> float -> t

val dim : t -> int
val is_empty : t -> bool

val linspace : ?v:t -> float -> float -> int -> t
val logspace : ?v:t -> float -> float -> int -> t

val of_array : float array -> t
val to_array : t -> float array

val of_list : float list -> t
val to_list : t -> float list

val append : t -> t -> t
val concat : t list -> t

val iter : (float -> unit) -> t -> unit
val iteri : (int -> float -> unit) -> t -> unit
val map : (float -> float) -> ?v:t -> t -> t
val fold : ('a -> float -> 'a) -> 'a -> t -> 'a
val for_all : (float -> bool) -> t -> bool

val iter2 : (float -> float -> unit) -> t -> t -> unit
val iter2i : (int -> float -> float -> unit) -> t -> t -> unit
val map2 : (float -> float -> float) -> ?v:t -> t -> t -> t
val fold2 : ('a -> float -> float -> 'a) -> 'a -> t -> t -> 'a
val for_all2 : (float -> float -> bool) -> t -> t -> bool

val same : ?eps:float -> t -> t -> bool

val rev : t -> t
val sorted : ?cmp:(float -> float -> int) -> ?v:t -> t -> t

val arg_min : t -> int
val arg_max : t -> int

(* binary transforms *)
val add : ?v:t -> t -> t -> t
val sub : ?v:t -> t -> t -> t
val mult : ?v:t -> t -> t -> t
val div : ?v:t -> t -> t -> t

(* binary reductions *)
val dot : t -> t -> float

(* unary transforms *)
val scale : ?v:t -> t -> float -> t
val recip : ?v:t -> t -> t
val scalar_add: ?v:t -> t -> float -> t
val scalar_sub: ?v:t -> t -> float -> t
val neg : ?v:t -> t -> t
val abs : ?v:t -> t -> t
val sign : ?v:t -> t -> t

(* unary reductions *)
val min : t -> float
val max : t -> float
val sum : t -> float
val prod : t -> float
val l1_norm : t -> float
val l2_norm : t -> float
val chebyshev_norm : t -> float
val minkowski_norm : t -> float -> float

val normalize : ?v:t -> t -> t

val linearize : ?v:t -> t -> float -> float -> t
val cross3d : t -> t -> t
val cosine_similarity : t -> t -> float

(* io *)
val pp : t -> unit
