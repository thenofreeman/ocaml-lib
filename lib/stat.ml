
let mean xs =
  (Vector.sum xs) /. Float.of_int (Vector.dim xs)

let std_dev xs =
  let mean = mean xs in
  Vector.l2Norm (Vector.scalar_sub xs mean)
    /. sqrt (Float.of_int (Vector.dim xs - 1))
