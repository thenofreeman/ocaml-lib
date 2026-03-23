module Manip = struct
  let quad_kernel x1 x2 =
    let x1x2 = (Vector.dot x1 x2) in
    let x1_2 = (Vector.dot x1 x1) in
    let x2_2 = (Vector.dot x2 x2) in
    (x1_2, x2_2, x1x2 *. Const.sqrt2)

  let rbf_kernel ~gamma x1 x2 =
    exp (gamma *. (Vector.sub x1 x2 |> Vector.l2_norm))

  let normalize xs =
    let max = Vector.max xs in
    let min = Vector.min xs in
    let range = (max -. min) in
    Vector.map (fun x -> (x -. min) /. range) xs

  let standardize xs =
    let mean = Stat.mean xs in
    let std_dev = Stat.std_dev xs in
    Vector.map (fun x -> (x -. mean) /. std_dev) xs

  let one_hot features =
    let n = List.length features in
    fst (List.fold_left (fun (acc, i) _ ->
        let arr = Array.make n 0 in
        arr.(i) <- 1; (arr :: acc, i+1)
      ) ([], 0) features)

  let bucket_partition ?(cmp=( < )) features partitions =
    let n = List.length partitions in
    Array.of_list
        (List.map (fun x ->
             match List.find_opt (fun p -> cmp x p) partitions with
             | None -> n
             | Some p -> p
           ) features
        )

  (* some sort of bucketing based off a distribution? etc *)
  (* let bucket_dist features xxxx = () *)

  let discretize xs steps =
    Vector.map (fun x ->
        List.fold_left (fun min s ->
            if x -. s > x -. min then min
            else x
          ) (List.hd steps) steps
      ) xs
end
