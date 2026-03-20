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
  let xs, ys = load_data "Advertising.csv" in

  let w, b = Learning.fit_linear_regression xs ys in

  Printf.printf "%f\n" (w *. 23.0 +. b)
