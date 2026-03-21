open Ocamllib

let load_mnist image_file label_file =
  let (n_images, n_rows, n_cols, image_data) = Read.read_image_bin image_file 2051 in
  let labels = Read.read_file_bin label_file 2049 in

  let features = Matrix.build n_images (n_rows * n_cols) image_data in
  let labels = Vector.build (Array.map float_of_int labels) in
  (features, labels)

let () =
  let x_train, y_train =
    load_mnist
      "sample_data/mnist/train-images.idx3-ubyte"
      "sample_data/mnist/train-labels.idx1-ubyte"
  in
  let x_test, y_test =
    load_mnist
      "sample_data/mnist/t10k-images.idx3-ubyte"
      "sample_data/mnist/t10k-labels.idx1-ubyte"
  in

  let n_correct = ref 0 in

  (* uses a bad example with k=50 to demostrate some failures *)

  let n = 10 in
  for i = 0 to n-1 do
    let query = Matrix.get_row x_test i in
    let actual = Vector.get y_test i in
    let prediction = Learn.Model.k_nearest_neighbors
                        x_train y_train query 50 in

    Printf.printf "%.0f -- %.0f\n%!" prediction actual;
    if Float.equal prediction actual then
      n_correct := !n_correct + 1
  done;
  Printf.printf "Accuracy: %.2f%%\n" Float.(of_int !n_correct /. of_int n)
