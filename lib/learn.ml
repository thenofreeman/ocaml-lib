module Solver = struct
  let batch_grad ~dl_dw ~dl_db ?(w=0.0) ?(b=0.0) ?(alpha=0.001) ?(epochs=15000) xs ys =
    let n = Float.of_int (Vector.dim xs) in
    let alpha_n = alpha /. n in

    let update w b =
      let dl_dw' = Vector.sum (Vector.map2 (dl_dw w b) xs ys) in
      let dl_db' = Vector.sum (Vector.map2 (dl_db w b) xs ys) in

      let w, b = (w -. alpha_n *. dl_dw',
                  b -. alpha_n *. dl_db')
      in (w, b)
    in

    let rec train w b i =
      if i = 0 then w, b
      else
        let w, b = update w b in
        train w b (i-1)

    in train w b epochs

  (* let stochastic_grad xs ys = () *)
  (* let minibatch_grad xs ys = () *)
end

module Model = struct
  let fit_linear_regression ~solver xs ys =
    solver xs ys

  (* let fit_logistic_regression ~solver xs ys = *)
  (*   () *)

  let k_nearest_neighbors
      ?(metric=fun a b -> Vector.l2_norm (Vector.sub a b))
      features labels q k =
    if k <= 0 then invalid_arg "k must be > 0";

    let rows = Array.of_list (Matrix.row_vecs features) in
    let n = Array.length rows in
    if Vector.dim labels <> n then
      invalid_arg "mismatch label and n";

    let dists = Array.mapi (fun i x -> (metric x q, i)) rows in
    Array.sort (fun (da, _) (db, _) -> Float.compare da db) dists;

    let k = Int.min k n in
    let k_labels = Array.init k (fun i ->
        let _, idx = dists.(i) in
        Vector.get labels idx
      ) in
    Stat.mode (Vector.of_array k_labels)

  (* let fit_logistic_regression ~solver xs ys = () *)
end

module Assess = struct
  let precision conf =
    let tp, _, fp, _ = conf in
    tp /. (tp +. fp)

  let recall conf =
    let tp, fn, _, _ = conf in
    tp /. (tp +. fn)

  let accuracy conf =
    let tp, fn, fp, tn = conf in
    let t, f = (tp +. tn, fp +. fn) in
    t /. (t +. f)

  let fpr conf =
    let _, _, fp, tn = conf in
    fp /. (fp +. tn)

  (* TODO: area under the roc curve *)
  (* requires differentiation *)
  (* let auc conf = () *)

  (* aliases *)
  let tpr conf = recall conf
end
