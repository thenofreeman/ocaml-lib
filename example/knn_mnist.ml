open Ocamllib

let read_u8 ic =
  input_byte ic

let read_be_i32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

let read_mnist_labels filename =
  let ic = open_in_bin filename in
  try
    let magic = read_be_i32 ic in
    if magic <> 2049 then
      invalid_arg "read_mnist_labels: bad magic number";

    let count = read_be_i32 ic in
    let labels = Array.init count (fun _ -> read_u8 ic) in
    close_in ic;
    labels
  with e ->
    close_in_noerr ic;
    raise e

let read_mnist_images filename =
  let ic = open_in_bin filename in
  try
    let magic = read_be_i32 ic in
    if magic <> 2051 then
      invalid_arg "read_mnist_images: bad magic number";

    let count = read_be_i32 ic in
    let rows = read_be_i32 ic in
    let cols = read_be_i32 ic in
    let pixels_per_image = rows * cols in

    let data = Array.make (count * pixels_per_image) 0.0 in

    for i = 0 to count * pixels_per_image - 1 do
      data.(i) <- float_of_int (read_u8 ic) /. 255.0
    done;

    close_in ic;
    (count, rows, cols, data)
  with e ->
    close_in_noerr ic;
    raise e

let load_mnist image_file label_file =
  let (n_images, n_rows, n_cols, image_data) = read_mnist_images image_file in
  let labels = read_mnist_labels label_file in

  if Array.length labels <> n_images then
    invalid_arg "load_mnist: image/label count mismatch";

  let features =
    Matrix.build n_images (n_rows * n_cols) image_data
  in

  let label_vec =
    Vector.build (Array.map float_of_int labels)
  in
  (features, label_vec)

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

  Random.self_init ();
  (* let rand_image_idx = Random.int (Matrix.rows x_train) in *)
  let rand_image_idx = Random.int 100 in

  let query = Matrix.get_row x_test rand_image_idx in
  let actual = Vector.get y_test rand_image_idx in

  let prediction = Learn.Model.k_nearest_neighbors
                     x_train y_train query 3 in

  Printf.printf "%f -- %f\n" prediction actual
