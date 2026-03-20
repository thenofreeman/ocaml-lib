open Ocamllib

let load_data filename =
  let rows = List.tl (Csv.load filename) in
  let data =
    List.map (fun row ->
      let x = float_of_string (List.nth row 2) in
      let y = float_of_string (List.nth row 4) in
      (x, y)
    ) rows
  in
  (Vector.of_list (List.map fst data), Vector.of_list (List.map snd data))

let () =
  let xs, ys = load_data "sample_data/Advertising.csv" in

  let batch_grad =
    Learn.Solver.batch_grad
      ~dl_dw:(fun w b x y -> (-2.0) *. x *. (y -. (w *. x +. b)))
      ~dl_db:(fun w b x y -> (-2.0) *. (y -. (w *. x +. b))) in

  let w, b = Learn.Model.fit_linear_regression ~solver:batch_grad xs ys in

  Printf.printf "%f\n" (w *. 23.0 +. b)
