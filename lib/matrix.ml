(* matrix.ml *)

module Ld = Lacaml.D
module Lmat = Lacaml.D.Mat

type t = Ld.mat

let create m n =
  Lmat.create m n

let make m n k =
  Lmat.make m n k

let init m n f =
  Lmat.init_rows m n (fun i j -> f ((i - 1) * n + (j - 1)))

let zero m n =
  Lmat.make0 m n

let empty =
  Lmat.empty

let random ?(range = (-1.0, 1.0)) m n =
  let from, upto = range in
  Lmat.random ~from ~range:(upto -. from) m n

let diag n k =
  Lmat.of_diag (Vector.make n k)

let eye n =
  Lmat.identity n

let diag_rand ?(range = (-1.0, 1.0)) n =
  Lmat.of_diag (Vector.random ~range n)

let diag_init n f =
  Lmat.of_diag (Vector.init n f)

let get x i j =
  x.{i + 1, j + 1}

let set x i j k =
  x.{i + 1, j + 1} <- k;
  x

let nrows x =
  Lmat.dim1 x

let ncols x =
  Lmat.dim2 x

let dims x =
  (nrows x, ncols x)

let is_empty x =
  Lmat.has_zero_dim x

let assert_match_dims x y =
  let rx, cx = dims x in
  let ry, cy = dims y in
  if rx <> ry || cx <> cy then invalid_arg "dimensions don't match"

let row x i =
  Lmat.copy_row x (i + 1)

let col x j =
  Ld.copy (Lmat.col x (j + 1))

let of_array r c a =
  let n = Array.length a in
  if n <> r * c then invalid_arg "Matrix.of_array: incompatible dimensions";
  Lmat.init_rows r c (fun i j -> a.((i - 1) * c + (j - 1)))

let to_array x =
  let rows = Lmat.to_array x in
  let r = Array.length rows in
  if r = 0 then [||]
  else
    let c = Array.length rows.(0) in
    Array.init (r * c) (fun k ->
        let i = k / c in
        let j = k mod c in
        rows.(i).(j))

let of_list r c xs =
  of_array r c (Array.of_list xs)

let to_list x =
  Array.to_list (to_array x)

let row_vecs x =
  let r = nrows x in
  List.init r (fun i -> row x i)

let col_vecs x =
  let c = ncols x in
  List.init c (fun j -> col x j)

let iteri f x =
  let r, c = dims x in
  for k = 0 to (r * c) - 1 do
    let i = k / c in
    let j = k mod c in
    f k (get x i j)
  done

let iter f x =
  iteri (fun _ xi -> f xi) x

let map f ?v x =
  match v with
  | None -> Lmat.map f x
  | Some v -> Lmat.map ~b:v f x

let fold f init x =
  let acc = ref init in
  iter (fun xi -> acc := f !acc xi) x;
  !acc

let for_all f x =
  let r, c = dims x in
  let rec loop k =
    if k >= (r * c) then true
    else
      let i = k / c in
      let j = k mod c in
      if not (f (get x i j)) then false
      else loop (k + 1)
  in
  loop 0

let iter2i f x y =
  assert_match_dims x y;
  let r, c = dims x in
  for k = 0 to (r * c) - 1 do
    let i = k / c in
    let j = k mod c in
    f k (get x i j) (get y i j)
  done

let iter2 f x y =
  iter2i (fun _ xi yi -> f xi yi) x y

let map2 f ?v x y =
  assert_match_dims x y;
  let result =
    match v with
    | None -> create (nrows x) (ncols x)
    | Some v -> v
  in
  iter2i (fun k xi yi ->
      let c = ncols result in
      let i = k / c in
      let j = k mod c in
      ignore (set result i j (f xi yi))) x y;
  result

let fold2 f init x y =
  assert_match_dims x y;
  let acc = ref init in
  iter2i (fun _ xi yi ->
      acc := f !acc xi yi
    ) x y;
  !acc

let for_all2 f x y =
  assert_match_dims x y;
  let r, c = dims x in
  let rec loop k =
    if k >= (r * c) then true
    else
      let i = k / c in
      let j = k mod c in
      if not (f (get x i j) (get y i j)) then false
      else loop (k + 1)
  in
  loop 0

let same ?(eps = 1e-9) x y =
  let rx, cx = dims x in
  let ry, cy = dims y in
  if rx <> ry || cx <> cy then false
  else for_all2 (fun xi yi -> Float.abs (xi -. yi) <= eps) x y

let copy ?v x =
  match v with
  | None -> Lmat.map (fun xi -> xi) x
  | Some v -> Lmat.map ~b:v (fun xi -> xi) x

let vectorize ?v x =
  match v with
  | None -> Ld.copy (Lmat.as_vec x)
  | Some v -> Ld.copy ~y:v (Lmat.as_vec x)

let devectorize v r c =
  if Vector.dim v <> r * c then
    invalid_arg "Matrix.devectorize: incompatible dimensions";
  let m = create r c in
  ignore (Ld.copy ~y:(Lmat.as_vec m) v);
  m

let transpose ?v x =
  match v with
  | None -> Lmat.transpose_copy x
  | Some v -> Lmat.transpose_copy ~b:v x

let inv ?v x =
  let r, c = dims x in
  if r <> c then invalid_arg "Matrix.inv: non-square matrix";
  let y = copy ?v x in
  Ld.getri y;
  y

let trace x =
  Lmat.trace x

let perm ?v x (i, j) =
  let y = copy ?v x in
  let r = nrows y in
  if i < 0 || j < 0 || i >= r || j >= r then
    invalid_arg "Matrix.perm: row index out of bounds";
  Lmat.swap ~m:1 ~n:(ncols y) ~ar:(i + 1) ~ac:1 y ~br:(j + 1) ~bc:1 y;
  y

let solve a b =
  let a' = copy a in
  let x = copy b in
  Ld.gesv a' x;
  x

let lu x =
  let lu = copy x in
  let ipiv = Ld.getrf lu in
  (lu, ipiv)

let qr x =
  let a = copy x in
  let m, n = dims a in
  let k = Int.min m n in
  let tau = Ld.geqrf a in

  let q = create m k in
  for i = 0 to m - 1 do
    for j = 0 to k - 1 do
      ignore (set q i j (get a i j))
    done
  done;
  if k > 0 then Ld.orgqr ~m ~n:k ~k ~tau q;

  let r = zero k n in
  for i = 0 to k - 1 do
    for j = i to n - 1 do
      ignore (set r i j (get a i j))
    done
  done;

  (q, r)

let cholesky ?(upper = true) ?v x =
  let y = copy ?v x in
  Ld.potrf ~up:upper y;
  y

let svd ?(full = false) x =
  let a = copy x in
  let job = if full then `A else `S in
  let s, u, vt = Ld.gesvd ~jobu:job ~jobvt:job a in
  (u, s, vt)

let eigvals_sym ?(upper = true) x =
  let a = copy x in
  Ld.syev ~vectors:false ~up:upper a

let eig_sym ?(upper = true) x =
  let a = copy x in
  let w = Ld.syev ~vectors:true ~up:upper a in
  (w, a)

let scale ?v k x =
  let result = copy ?v x in
  Lmat.scal k result;
  result

let recip ?v x =
  match v with
  | None -> Lmat.reci x
  | Some v -> Lmat.reci ~b:v x

let scalar_add ?v x k =
  match v with
  | None -> Lmat.add_const k x
  | Some v -> Lmat.add_const ~b:v k x

let scalar_sub ?v x k =
  match v with
  | None -> Lmat.add_const (-.k) x
  | Some v -> Lmat.add_const ~b:v (-.k) x

let neg ?v x =
  match v with
  | None -> Lmat.neg x
  | Some v -> Lmat.neg ~b:v x

let abs ?v x =
  match v with
  | None -> Lmat.abs x
  | Some v -> Lmat.abs ~b:v x

let sign ?v x =
  match v with
  | None -> Lmat.signum x
  | Some v -> Lmat.signum ~b:v x

let add ?v x y =
  match v with
  | None -> Lmat.add x y
  | Some v -> Lmat.add ~c:v x y

let sub ?v x y =
  match v with
  | None -> Lmat.sub x y
  | Some v -> Lmat.sub ~c:v x y

let mult ?v x y =
  match v with
  | None -> Lmat.mul x y
  | Some v -> Lmat.mul ~c:v x y

let div ?v x y =
  match v with
  | None -> Lmat.div x y
  | Some v -> Lmat.div ~c:v x y

let dot x y =
  Ld.gemm x y

let kron x y =
  let rx, cx = dims x in
  let ry, cy = dims y in
  let z = zero (rx * ry) (cx * cy) in
  for i = 0 to rx - 1 do
    for j = 0 to cx - 1 do
      let xij = get x i j in
      for p = 0 to ry - 1 do
        for q = 0 to cy - 1 do
          ignore (set z (i * ry + p) (j * cy + q) (xij *. get y p q))
        done
      done
    done
  done;
  z

let hadamard ?v x y =
  mult ?v x y

let sum_rows x =
  let r, c = dims x in
  if c = 0 then Vector.zero r
  else Ld.gemv x (Vector.make c 1.0)

let sum_cols x =
  let r, c = dims x in
  if r = 0 then Vector.zero c
  else Ld.gemv ~trans:`T x (Vector.make r 1.0)

let mean_rows x =
  let _, c = dims x in
  if c = 0 then sum_rows x
  else Vector.scale (1.0 /. Float.of_int c) (sum_rows x)

let mean_cols x =
  let r, _ = dims x in
  if r = 0 then sum_cols x
  else Vector.scale (1.0 /. Float.of_int r) (sum_cols x)

let pp (x : t) =
  Format.printf "%a@." (Lacaml.Io.pp_lfmat ()) x
