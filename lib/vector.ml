type t = {
  data : float array;
  len : int
}

exception Mismatched_dimensions of string
exception Zero_vector

let build_vec arr =
  { data = arr;
    len = Array.length arr }

let make n init =
  if n < 0 then invalid_arg "Vector.make: negative dimension";
  build_vec (Array.make n init)

let get vec i =
  vec.data.(i)

let set vec i x =
  vec.data.(i) <- x

let zero n =
  make n 0.0

let of_list l =
  build_vec (Array.of_list l)

let to_list vec =
  Array.to_list vec.data

let of_array arr =
  build_vec (Array.copy arr)

let to_array vec =
  Array.copy vec.data

let dim vec =
  vec.len

let dim_guard ?op a b =
  let da = dim a in
  let db = dim b in
  if da <> db then
    raise (Mismatched_dimensions
             (Printf.sprintf "%s: dimensions %d and %d do not match"
                (match op with
                 | Some oper -> oper
                 | None -> "_")
                da db))

let copy vec =
  build_vec (Array.copy vec.data)

let map f vec =
  build_vec (Array.map f vec.data)

let map2 f a b =
  dim_guard a b ~op:"map2";
  build_vec (Array.init (dim a) (fun i -> f (get a i) (get b i)))

let add a b =
  map2 ( +. ) a b

let sub a b =
  map2 ( -. ) a b

let scalar_add vec k =
  map (fun x -> x +. k) vec

let scalar_sub vec k =
  map (fun x -> x -. k) vec

let neg vec =
  map (fun x -> 0.0 -. x) vec

let scale k vec =
  map (fun x -> k *. x) vec

let linearize vec m b =
  map (fun x -> m *. x +. b) vec

let fold_left f init vec =
  Array.fold_left f init vec.data

let sum vec =
  fold_left ( +. ) 0.0 vec

let map_sum f vec =
  fold_left (fun acc x -> acc +. f x) 0.0 vec

let map2_sum f a b =
  sum (map2 f a b)

let prod vec =
  fold_left ( *. ) 1.0 vec

let dot a b =
  dim_guard a b ~op:"dot";
  let sum = ref 0.0 in
  Array.iter2 (fun a b -> sum := !sum +. (a *. b)) a.data b.data;
  !sum

let max vec =
  if dim vec = 0 then invalid_arg "max: empty vector";
  fold_left Float.max (get vec 0) vec

let arg_max vec =
  if dim vec = 0 then invalid_arg "arg_max: empty vector";
  let arg_max_helper (max_v, i, max_i) x =
    if Float.max max_v x = max_v then (max_v, i+1, i)
    else (x, i+1, max_i) in

  let _, _, max_i = fold_left arg_max_helper ((get vec 0), 0, 0) vec in
  max_i

let cross3d a b =
  if dim a <> 3 || dim b <> 3 then invalid_arg "cross3d: non-3D vector";
  let a, b = a.data, b.data in
  [|
    a.(1) *. b.(2) -. a.(2) *. b.(1);
    a.(2) *. b.(0) -. a.(0) *. b.(2);
    a.(0) *. b.(1) -. a.(1) *. b.(0);
  |]

let l1Norm vec =
  map_sum abs_float vec

let l2Norm vec =
  sqrt (dot vec vec)

let chebyshev_norm vec =
  if dim vec = 0 then invalid_arg "chebyshev_norm: empty vector";
  fold_left (fun acc x -> Float.max acc (abs_float x)) (abs_float (get vec 0)) vec

let minkowski_norm vec p =
  match p with
  | 1.0 -> l1Norm vec
  | 2.0 -> l2Norm vec
  | p -> begin
      (map_sum (fun x -> (abs_float x) ** p) vec) ** (1.0 /. p)
  end

let cosine_similarity a b =
  dim_guard a b ~op:"cosine_similarity";
  (dot a b) /. ((l2Norm a) *. (l2Norm b))

let unit vec =
  let n = l2Norm vec in
  if n = 0.0 then raise Zero_vector;
  scale (1.0 /. n) vec

let same ?(epsilon=0.001) a b =
  dim_guard a b ~op:"same";
  let rec loop i =
    if i = dim a then true
    else if Float.abs ((get a i) -. (get b i)) > epsilon then false
    else loop (i+1)
  in loop 0

let pp vec =
  let parts = Array.to_list (Array.map string_of_float vec.data) in
  "[" ^ String.concat "; " parts ^ "]"

(* aliases *)
let manhattan_norm vec = l1Norm vec
let taxicab_norm vec = l1Norm vec
let euclidean_norm vec = l2Norm vec
let magnitude vec = l2Norm vec
