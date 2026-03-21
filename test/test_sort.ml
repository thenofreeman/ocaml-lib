open OUnit2
open Ocamllib

let build_randomized_test () =
  Random.self_init ();
  let arr = Array.init (Random.int 1000) (fun _ -> Random.int 1000) in
  let sorted = Array.copy arr in
  Array.sort Int.compare sorted;
  (arr, sorted)

let randomized_test = build_randomized_test ()

let make_arr_comparison_test name ~op arr result =
  name >:: (fun _ -> assert_equal ~cmp:(Array.equal Int.equal) result (op ~cmp:( >= ) arr))

let make_fn_purity_test name ~op arr =
  name >:: (fun _ -> assert (arr != op ~cmp:( >= ) arr))

let build_sort_test_suite name algo =
  [
    make_arr_comparison_test (Printf.sprintf "%s_empty" name) ~op:algo
      [| |]
      [| |];
    make_arr_comparison_test (Printf.sprintf "%s_single_element" name) ~op:algo
      [| 10 |]
      [| 10 |];
    make_arr_comparison_test (Printf.sprintf "%s_two_element" name) ~op:algo
      [| 10; 2 |]
      [| 2; 10 |];
    make_arr_comparison_test (Printf.sprintf "%s_presorted" name) ~op:algo
      [| 1; 2; 3; 4 |]
      [| 1; 2; 3; 4 |];
    make_arr_comparison_test (Printf.sprintf "%s_reversed" name) ~op:algo
      [| 4; 3; 2; 1 |]
      [| 1; 2; 3; 4 |];
    make_arr_comparison_test (Printf.sprintf "%s_hasduplicate" name) ~op:algo
      [| 3; 1; 2; 1; 3 |]
      [| 1; 1; 2; 3; 3 |];
    make_fn_purity_test (Printf.sprintf "%s_purity" name) ~op:algo
      [| 3; 1; 2|];
    make_arr_comparison_test (Printf.sprintf "%s_randomized" name) ~op:algo
      (fst randomized_test)
      (snd randomized_test);
  ]

let test_sort = "sort test suite" >::: List.flatten [
    build_sort_test_suite "insertion_sort" Algo.Sort.insertion;
  ]

let _ = run_test_tt_main test_sort
