let batch_grad ?(w=0.0) ?(b=0.0) ?(alpha=0.001) ?(epochs=15000) xs ys =
  Vector.dim_guard xs ys ~op:"gradient_descent";

  let n = Float.of_int (Vector.dim xs) in

  let update w b =
    let dl_dw = Vector.map2_sum
        (fun x y -> (-2.0) *. x *. (y -. (w *. x +. b)))
        xs ys
    in

    let dl_db = Vector.map2_sum
        (fun x y -> (-2.0) *. (y -. (w *. x +. b)))
        xs ys
    in

    let w, b = (w -. (1.0 /. n) *. dl_dw *. alpha,
                b -. (1.0 /. n) *. dl_db *. alpha)
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

let fit_linear_regression ?(solver=batch_grad) xs ys =
  solver xs ys
