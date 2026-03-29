open Owl_maths

let cdf x = Owl_stats.gaussian_cdf ~mu:0.0 ~sigma:1.0 x

let black_scholes r s k t sigma =
  let d1 =
    (log (s /. k) +. (r +. 0.5 *. sigma *. sigma) *. t)
    *. (sqrt t) /. sigma in
  let d2 = d1 -. sigma *. (sqrt t) in
  (cdf d1) *. s -. (cdf d2) *. k *. (exp (-.r *. t))
;;
