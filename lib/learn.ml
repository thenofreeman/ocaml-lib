module Solver = struct
  let batch_grad ~dl_dw ~dl_db ?(w=0.0) ?(b=0.0) ?(alpha=0.001) ?(epochs=15000) xs ys =
    Vector.dim_guard xs ys ~op:"gradient_descent";

    let n = Float.of_int (Vector.dim xs) in
    let alpha_n = alpha /. n in

    let update w b =
      let dl_dw' = Vector.map2_sum (dl_dw w b) xs ys in
      let dl_db' = Vector.map2_sum (dl_db w b) xs ys in

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

  let k_nearest_neighbors
      ?(metric=fun a b -> Vector.l2Norm (Vector.sub a b))
      features labels q k =
    let dists_with_idx = Matrix.mapi_rows (fun i x ->
        (metric x q, i)
      ) features in
    let k_nearest = Array.sub (
        Array.of_list (List.sort (fun (a, _) (b, _) -> Float.compare a b) dists_with_idx)
      ) 0 k in

    let k_labels = Array.map (fun (_, i) ->
        Vector.get labels i
      ) k_nearest in

    Stat.mode (Vector.build k_labels)

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
