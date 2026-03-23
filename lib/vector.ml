(* vector.ml *)

module Ld = Lacaml.D
module Lvec = Lacaml.D.Vec

type t = Ld.vec

let create n =
  Lvec.create n

let make n k =
  Lvec.make n k

let init n f =
  Lvec.init n f

let zero n =
  Lvec.make0 n

let empty =
  Lvec.empty

let random ?(range=(-1., 1.)) n =
  let from, upto = range in
  Lvec.random ~from ~range:(upto -. from) n

let get x i =
  x.{i + 1}

let set x i k =
  x.{i + 1} <- k; x

let dim x =
  Lvec.dim x

let is_empty x =
  Lvec.has_zero_dim x

let linspace ?v a b n =
  match v with
  | None -> Lvec.linspace a b n
  | Some v -> Lvec.linspace ~y:v a b n

let logspace ?v a b n =
  match v with
  | None -> Lvec.logspace a b n
  | Some v -> Lvec.logspace ~y:v a b n

let of_array x =
  Lvec.of_array x

let to_array x =
  Lvec.to_array x

let of_list l =
  Lvec.of_list l

let to_list x =
  Lvec.to_list x

let append x y =
  Lvec.append x y

let concat xs =
  Lvec.concat xs

let iter f x =
  Lvec.iter f x

let iteri f x =
  Lvec.iteri f x

let map f ?v x =
  match v with
  | None -> Lvec.map f x
  | Some v -> Lvec.map ~y:v f x

let fold f init x =
  Lvec.fold f init x

let for_all f x =
  let n = dim x in
  let rec loop i =
    if i > n then true
    else if not (f (get x i)) then false
    else loop (i + 1)
  in
  loop 0

let assert_match_dim x y =
  let n = dim x in
  if dim y <> n then invalid_arg "dimensions don't match"

let iter2i f x y =
  assert_match_dim x y;
  let n = dim x in
  for i = 0 to n do
    f i (get x i) ( get y i)
  done

let iter2 f x y =
  assert_match_dim x y;
  iter2i (fun _ xi yi ->
      f xi yi
    ) x y

let map2 f ?v x y =
  assert_match_dim x y;
  let result = match v with
    | None -> create (dim x)
    | Some v -> v in
  iter2i (fun i xi yi ->
      ignore (set result i (f xi yi))
    ) x y;
  result

let fold2 f init x y =
  assert_match_dim x y;
  let acc = ref init in
  iter2i (fun _ xi yi ->
      acc := f !acc xi yi
    ) x y;
  !acc

let for_all2 f x y =
  assert_match_dim x y;
  let n = dim x in
  let rec loop i =
    if i > n then true
    else if not (f (get x i) (get y i)) then false
    else loop (i + 1)
  in
  loop 0

let same ?(eps=1e-9) x y =
  let n = dim x in
  if dim y <> n then false
  else for_all2 (fun xi yi -> xi -. yi > eps) x y

let rev x =
  Lvec.rev x

let copy ?v x =
  match v with
  | None -> Ld.copy x
  | Some v -> Ld.copy ~y:v x

let sorted ?(cmp=Float.compare) ?v x =
  match v with
  | None ->
    let result = copy x in
    Lvec.sort ~cmp result; result
  | Some v ->
    Lvec.sort ~cmp v; v

let arg_min x =
  if is_empty x then invalid_arg "Vector.arg_max";

  let min_i = ref 0 in
  iteri (fun i xi ->
      if xi <= (get x !min_i) then min_i := i;
    ) x;
  !min_i

let arg_max x =
  if is_empty x then invalid_arg "Vector.arg_max";

  let max_i = ref 0 in
  iteri (fun i xi ->
      if xi >= (get x !max_i) then max_i := i;
    ) x;
  !max_i

let add ?v x y =
  match v with
  | None -> Lvec.add x y
  | Some v -> Lvec.add ~z:v x y

let sub ?v x y =
  match v with
  | None -> Lvec.sub x y
  | Some v -> Lvec.sub ~z:v x y

let mult ?v x y =
  match v with
  | None -> Lvec.mul x y
  | Some v -> Lvec.mul ~z:v x y

let div ?v x y =
  match v with
  | None -> Lvec.div x y
  | Some v -> Lvec.div ~z:v x y

let dot x y =
  Ld.dot x y

let scale ?v k x =
  match v with
  | None ->
    let result = create (dim x) in
    Ld.scal k result; result
  | Some v -> Ld.scal k v; v

let recip ?v x =
  match v with
  | None -> Lvec.reci x
  | Some v -> Lvec.reci ~y:v x

let scalar_add ?v x k =
  match v with
  | None -> Lvec.add_const k x
  | Some v -> Lvec.add_const ~y:v k x

let scalar_sub ?v x k =
  match v with
  | None -> Lvec.add_const (-.k) x
  | Some v -> Lvec.add_const ~y:v (-.k) x

let neg ?v x =
  match v with
  | None -> Lvec.neg x
  | Some v -> Lvec.neg ~y:v x

let abs ?v x =
  match v with
  | None -> Lvec.abs x
  | Some v -> Lvec.abs ~y:v x

let sign ?v x =
  match v with
  | None -> Lvec.signum x
  | Some v -> Lvec.signum ~y:v x

let min x =
  Lvec.min x

let max x =
  Lvec.max x

let sum x =
  Lvec.sum x

let prod x =
  Lvec.prod x

let l1_norm x =
  sum (abs x)

let l2_norm x =
  Lvec.sqr_nrm2 ~stable:true x

let chebyshev_norm x =
  fold (fun acc x ->
      Float.max acc (Float.abs x)
    ) (Float.abs (get x 0)) x

let minkowski_norm x p =
  match p with
  | 1.0 -> l1_norm x
  | 2.0 -> l2_norm x
  | p -> sum (map (fun x -> (Float.abs x) ** p) x)

let normalize ?v x =
  match v with
  | None -> scale (1. /. (l2_norm x)) x
  | Some v -> scale ~v:v (1. /. (l2_norm x)) x

let linearize ?v x m b =
  match v with
  | None -> map (fun x -> m *. x +. b) x
  | Some v -> map ~v:v (fun x -> m *. x +. b) x

let cross3d x y =
  if dim x <> 3 || dim y <> 3 then invalid_arg "cross3d: non-3D vector";
  of_array [|
    (get x 1) *. (get y 2) -. (get x 2) *. (get y 1);
    (get x 2) *. (get y 0) -. (get x 0) *. (get y 2);
    (get x 0) *. (get y 1) -. (get x 1) *. (get y 0);
  |]

let cosine_similarity x y =
  assert_match_dim x y;
  (dot x y) /. ((l2_norm x) *. (l2_norm y))

let pp (x : t) =
  Format.printf "%a@." (Lacaml.Io.pp_lfvec ()) x
