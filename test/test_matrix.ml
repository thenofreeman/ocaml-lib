open OUnit2
open Ocamllib

module MatrixTest = struct
  module Make = struct
    (* let unary_oper_test ~oper name vec result = *)
    (*   name >:: (fun _ -> assert_equal ~cmp:Vector.same result (oper vec)) *)

    (* let unary_fold_test ~oper name vec result = *)
    (*   name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper vec)) *)

    let binary_oper_test ~oper name a b result =
      name >:: (fun _ -> assert_equal ~cmp:Matrix.same result (oper a b))

    (* let binary_fold_test ~oper name a b result = *)
    (*   name >:: (fun _ -> assert_equal ~cmp:Float.equal result (oper a b)) *)

    (* let validate_length_test name vec len = *)
    (*   name >:: (fun _ -> assert_equal (Vector.dim vec) len) *)

    (* let expect_fail_test name expr = *)
    (*     name >:: (fun _ -> assert_bool "" (try ignore (expr ()); false with | _ -> true)) *)
  end
end

let test_mat = "matrix test suite" >::: [
    (* binary operations *)
    MatrixTest.Make.binary_oper_test "kron_equals" ~oper:Matrix.kron
      (Matrix.build 2 2 [| 1.; 2.; 3.; 4. |])
      (Matrix.build 2 2 [| 0.; 5.; 6.; 7. |])
      (Matrix.build 4 4 [| 0.;  5.;  0.; 10.;
                           6.;  7.; 12.; 14.;
                           0.; 15.;  0.; 20.;
                          18.; 21.; 24.; 28. |]);
  ]

let _ = run_test_tt_main test_mat
