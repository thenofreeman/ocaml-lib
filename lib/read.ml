
let read_be_i32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

let read_file_bin filename magic =
  let ic = open_in_bin filename in
  try
    if read_be_i32 ic <> magic then
      invalid_arg "read_file_bin: bad magic number";

    let count = read_be_i32 ic in
    let labels = Array.init count (fun _ -> input_byte ic) in
    close_in ic;
    labels
  with e ->
    close_in_noerr ic;
    raise e

let read_image_bin filename magic =
  let ic = open_in_bin filename in
  try
    if read_be_i32 ic <> magic then
      invalid_arg "read_image_bin: bad magic number";

    let count = read_be_i32 ic in
    let rows = read_be_i32 ic in
    let cols = read_be_i32 ic in
    let pixels_per_image = rows * cols in

    let data = Array.make (count * pixels_per_image) 0.0 in

    for i = 0 to count * pixels_per_image - 1 do
      data.(i) <- float_of_int (input_byte ic) /. 255.0
    done;

    close_in ic;
    (count, rows, cols, data)
  with e ->
    close_in_noerr ic;
    raise e
