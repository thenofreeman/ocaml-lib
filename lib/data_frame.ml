module DataFrame = struct
  type 'a t = {
    data : 'a array;
    rows : int;
    cols : int
  }

  let to_matrix df =
    let nrows = List.length l in
    let ncols = List.length (List.hd l) in
    let mat = make nrows ncols 0.0 in
    List.iteri (fun i row ->
        if List.length row <> ncols then invalid_arg "of_matrix: data_frame is jagged";
        let row = Array.of_list (List.map (fun x -> Float.of_string x) row) in
        Array.blit mat.data nrows row nrows ncols
      ) l;
    mat

  (* TODO *)
  (* let of_matrix mat = () *)
end
