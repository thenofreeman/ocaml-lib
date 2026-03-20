module Data = struct
  let to_matrix l =
    let nrows = List.length l in
    let ncols = List.length (List.hd l) in
    let mat = make nrows ncols 0.0 in
    List.iteri (fun i row ->
        if List.length row <> ncols then invalid_arg "of_data_matrix: data_matrix is jagged";
        let row = Array.of_list (List.map (fun x -> Float.of_string x) row) in
        Array.blit mat.data nrows row nrows ncols
      ) l;
    mat

  (* TODO *)
  (* let of_matrix mat = () *)

  let from_csv_float ?(hasheader=false) filename =
    (* TODO: skip headers if hasheader is true *)
    to_matrix (Csv.load filename)
end
