open OUnit2
open Ocamllib

module VectorTest = struct
  module Make = struct
    let unary_oper_test ~oper name a result =
      name >:: (fun _ -> assert_equal ~cmp:Vector.same result (oper a))

    let unary_fold_test ~oper name a result =
      name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper a))

    let binary_oper_test ~oper name a b result =
      name >:: (fun _ -> assert_equal ~cmp:Vector.same result (oper a b))

    let binary_fold_test ~oper name a b result =
      name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper a b))
  end

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
end

let test_vec = "vector test suite" >::: [
    (* structure *)
    "invalid_length" >:: VectorTest.invalid_length;
    "valid_length" >:: VectorTest.valid_length;

    (* conversions *)

    (* unary operations *)
    VectorTest.Make.unary_oper_test "scaled_by_2_equals" ~oper:(Vector.scale 2.0)
      (Vector.make 3 1.0)
      (Vector.make 3 2.0);

    VectorTest.Make.unary_fold_test "sum_equals" ~oper:Vector.sum
      (Vector.make 3 1.0)
      3.0;

    (* binary operations *)
    VectorTest.Make.binary_oper_test "add_equals" ~oper:Vector.add
      (Vector.make 3 2.0)
      (Vector.make 3 3.0)
      (Vector.make 3 5.0);

    VectorTest.Make.binary_fold_test "dot_equals" ~oper:Vector.dot
      (Vector.make 3 2.0)
      (Vector.make 3 2.0)
      12.0
  ]

let _ = run_test_tt_main test_vec
