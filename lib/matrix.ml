type t = {
  data : float array;
  rows : int;
  cols : int
}

exception Mismatched_dimensions of string

let build_mat arr nrow ncol =
  if nrow * ncol <> Array.length arr then invalid_arg "build_mat: array size doesn't match dimensions m, n";
  { data = arr;
    rows = nrow;
    cols = ncol }

let make m n init =
  if n < 0 then invalid_arg "Matrix.make: negative dimension n";
  if m < 0 then invalid_arg "Matrix.make: negative dimension m";
  build_mat (Array.make (n * m) init) n m

let arr_idx mat i j =
  i * mat.cols + j

let mat_idx mat i =
  (i / mat.cols, i mod mat.cols)

let get mat i j =
  mat.data.(arr_idx mat i j)

let set mat i j x =
  mat.data.(arr_idx mat i j) <- x

let rows mat =
  mat.rows

let cols mat =
  mat.cols

let zero n m =
  make n m 0.0

let diag n init =
  let mat = make n n 0.0 in
  for i = 0 to n-1 do
    set mat i i init
  done;
  mat

let eye n =
  diag n 1.0

let diag_vector vec =
  let n = Vector.dim vec in
  let mat = make n n 0.0 in
  for i = 0 to n-1 do
    (set mat i i (Vector.get vec i))
  done;
  mat

(* TODO: this is from vector, need one for matrix *)
(* let dim_guard ?op a b = *)
(*   let da = dim a in *)
(*   let db = dim b in *)
(*   if da <> db then *)
(*     raise (Mismatched_dimensions *)
(*              (Printf.sprintf "%s: dimensions %d and %d do not match" *)
(*                 (match op with *)
(*                  | Some oper -> oper *)
(*                  | None -> "_") *)
(*                 da db)) *)

let transpose mat =
  let mat_t = make mat.rows mat.cols 0.0 in
  for i = 0 to mat.rows-1 do
    for j = 0 to mat.cols-1 do
      set mat_t j i (get mat i j)
    done
  done;
  mat_t

let of_col_vecs vec_list =
  let ncols = List.length vec_list in
  let nrows = Vector.dim (List.hd vec_list) in
  let mat = make nrows ncols 0.0 in
  List.iteri (fun i col ->
      if Vector.dim col <> nrows then invalid_arg "of_col_vecs: vec_list is jagged";
      Array.blit mat.data nrows col.data nrows ncols
    ) vec_list;
  transpose mat

(* let to_col_vecs mat = () *)

let of_row_vecs vec_list =
  let nrows = List.length vec_list in
  let ncols = Vector.dim (List.hd vec_list) in
  let mat = make nrows ncols 0.0 in
  List.iteri (fun i row ->
      if Vector.dim row <> ncols then invalid_arg "of_row_vecs: vec_list is jagged";
      Array.blit mat.data nrows row.data nrows ncols
    ) vec_list;
  mat

(* TODO *)
(* let to_row_vecs mat = () *)

let of_array arr =
  let nrows = Array.length arr in
  let ncols = Array.length (Array.get arr 0) in
  let mat = make nrows ncols 0.0 in
  Array.iteri (fun i row ->
      if Array.length row <> ncols then invalid_arg "of_array: 2d array is jagged";
      Array.blit mat.data nrows row nrows ncols
    ) arr;
  mat

(* TODO *)
(* let to_array mat = *)
(*   let arr_mat Array.make_matrix mat.rows mat.cols 0.0 in () *)

let of_list l =
  let nrows = List.length l in
  let ncols = List.length (List.hd l) in
  let mat = make nrows ncols 0.0 in
  List.iteri (fun i row ->
      if List.length row <> ncols then invalid_arg "of_list: 2d list is jagged";
      let row = Array.of_list row in
      Array.blit mat.data nrows row nrows ncols
    ) l;
  mat

(* TODO *)
(* let to_list mat = () *)

let of_flat_list l nrow ncol =
  build_mat (Array.of_list l) nrow ncol

let to_flat_list mat =
  Array.to_list mat

let of_flat_array arr nrow ncol =
  build_mat (Array.copy arr) nrow ncol

let to_flat_array mat =
  Array.copy mat.data
