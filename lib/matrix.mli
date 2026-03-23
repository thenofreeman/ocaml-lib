(* matrix.mli *)

type t = Lacaml.D.mat

val create : int -> int -> t
val make : int -> int -> float -> t
val init : int -> int -> (int -> float) -> t
val zero : int -> int -> t
val empty : t
val random : ?range:float*float -> int -> int -> t

val diag : int -> float -> t
val eye : int -> t
val diag_rand : ?range:float*float -> int -> t
val diag_init : int -> (int -> float) -> t

val get : t -> int -> int -> float
val set : t -> int -> int -> float -> t

val dims : t -> int * int
val is_empty : t -> bool
val same : ?eps:float -> t -> t -> bool

val row : t -> int -> Vector.t
val col : t -> int -> Vector.t

val nrows : t -> int
val ncols : t -> int

val of_array : int -> int -> float array -> t
val to_array : t -> float array

val of_list : int -> int -> float list -> t
val to_list : t -> float list

val row_vecs : t -> Vector.t list
val col_vecs : t -> Vector.t list

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

val copy : ?v:t -> t -> t

val vectorize : ?v:Vector.t -> t -> Vector.t
val devectorize : Vector.t -> int -> int -> t

val transpose : ?v:t -> t -> t
val inv : ?v:t -> t -> t
val trace : t -> float
val perm : ?v:t -> t -> int*int -> t
val solve : t -> t -> t

val lu : t -> t * Lacaml.Common.int32_vec
val qr : t -> t * t
val cholesky : ?upper:bool -> ?v:t -> t -> t
val svd : ?full:bool -> t -> t * Vector.t * t
val eigvals_sym : ?upper:bool -> t -> Vector.t
val eig_sym : ?upper:bool -> t -> Vector.t * t

val scale : ?v:t -> float -> t -> t
val recip : ?v:t -> t -> t
val scalar_add: ?v:t -> t -> float -> t
val scalar_sub: ?v:t -> t -> float -> t
val neg : ?v:t -> t -> t
val abs : ?v:t -> t -> t
val sign : ?v:t -> t -> t

val add : ?v:t -> t -> t -> t
val sub : ?v:t -> t -> t -> t
val mult : ?v:t -> t -> t -> t
val div : ?v:t -> t -> t -> t
val dot : t -> t -> t
val kron : t -> t -> t
val hadamard : ?v:t -> t -> t -> t

val sum_rows : t -> Vector.t
val sum_cols : t -> Vector.t
val mean_rows : t -> Vector.t
val mean_cols : t -> Vector.t

(* io *)
val pp : t -> unit
