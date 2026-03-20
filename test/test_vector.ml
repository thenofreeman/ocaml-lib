open OUnit2
open Ocamllib

module TestVector = struct
  let invalid_length =
    (fun _ ->
       assert_raises
         (Invalid_argument "Vector.make: negative dimension")
         (fun () -> Vector.make (-1) 0.0)
    )

  let valid_length =
    (fun _ ->
       let n = 5 in
       let v = Vector.make n 0.0 in
       assert_equal n (Vector.dim v)
    )

  let add_vectors =
    (fun _ ->
       let a = Vector.make 5 2.0 in
       let b = Vector.make 5 3.0 in
       assert_equal
         ~cmp:Vector.same
         (Vector.add a b) (Vector.make 5 5.0)
    )
end

let test_vec = "vector test suite" >::: [
    (* structure *)
    "invalid_length" >:: TestVector.invalid_length;
    "valid_length" >:: TestVector.valid_length;

    (* conversions *)

    (* operations *)
    "add_equal" >:: TestVector.add_vectors;



  ]

let _ = run_test_tt_main test_vec
