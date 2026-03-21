open Ocamllib

let () =
  let features = Matrix.build 6 2 [|
      2.; 3.;
      5.; 4.;
      9.; 6.;
      4.; 7.;
      8.; 1.;
      7.; 2.
    |] in

  let labels = Vector.build [|
      0.; 0.; 1.; 0.; 1.; 1.
    |] in

  let query = Vector.build [| 5.; 3. |] in

  let prediction = Learn.Model.k_nearest_neighbors
                     features labels query 3 in

  Printf.printf "%f\n" prediction
