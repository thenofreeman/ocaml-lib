open OUnit2
open Ocamllib

module VectorTest = struct
  module Make = struct
    let unary_oper_test ~oper name vec result =
      name >:: (fun _ -> assert_equal ~cmp:Vector.same result (oper vec))

    let unary_fold_test ~oper name vec result =
      name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper vec))

    let binary_oper_test ~oper name a b result =
      name >:: (fun _ -> assert_equal ~cmp:Vector.same result (oper a b))

    let binary_fold_test ~oper name a b result =
      name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper a b))

    let validate_length_test name vec len =
      name >:: (fun _ -> assert_equal (Vector.dim vec) len)

    let expect_fail_test name expr =
        name >:: (fun _ -> assert_bool "" (try ignore (expr ()); false with | _ -> true))
  end
end

let test_vec = "vector test suite" >::: [
    (* structure *)
    VectorTest.Make.validate_length_test "valid_length"
      (Vector.make 5 0.0)
      5;

    VectorTest.Make.expect_fail_test "invalid_length"
      (fun _ -> Vector.make (-1) 0.0);

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
