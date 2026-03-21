
let mean xs =
  (Vector.sum xs) /. Float.of_int (Vector.dim xs)

let std_dev xs =
  let mean = mean xs in
  Vector.l2Norm (Vector.scalar_sub xs mean)
    /. sqrt (Float.of_int (Vector.dim xs - 1))

let mode xs =
  let tbl = Hashtbl.create (2 * Vector.dim xs) in
  Vector.iter (fun x ->
      match Hashtbl.find_opt tbl x with
      | None -> Hashtbl.add tbl x 1.0
      | Some v -> Hashtbl.replace tbl x (v +. 1.0)
    ) xs;

  let first = Vector.get xs 0 in

  fst (Hashtbl.fold (fun k v (mk, mv) ->
      if v > mk then (k, v)
      else (mk, mv)
    ) tbl (first, Hashtbl.find tbl first))
