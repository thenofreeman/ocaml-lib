module Data = struct
  let quad_kernel x1 x2 =
    let x1x2 = (Vector.dot x1 x2) in
    let x1_2 = (Vector.dot x1 x1) in
    let x2_2 = (Vector.dot x2 x2) in
    (x1_2, x2_2, Vector.scale x1x2 Const.sqrt2)

  let rbf_kernel ~gamma x1 x2 =
    sub x1 x2
    |> Vector.l2Norm
    |> Vector.map (fun x -> exp (gamma *. x))
end
