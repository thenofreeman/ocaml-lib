(* Quicksort.ml *)

let quicksort arr =
  let rec quicksort arr p r =
    let select_pivot arr _ r = arr.(r) in

    let swap arr i j =
      let temp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- temp
    in

    let partition arr p r =
      let pivot = (select_pivot arr p r) in

      let i = ref p in
      for j = p to r-1 do
        if arr.(j) <= pivot then begin
          swap arr !i j;
          i := !i + 1
        end
      done;

      swap arr !i r;
      !i
    in

    if p < r then
      let pivot = partition arr p r in
      quicksort arr p (pivot-1);
      quicksort arr (pivot+1) r

  in quicksort arr 0 (Array.length arr - 1)

(* let%test "Quicksort" = *)
(*     let arr = [| 1; 5; 3; 6; 2; 9; 1 |] in *)
(*     let arr_sorted = Array.copy arr in *)

(*     quicksort arr_sorted; *)
(*     Array.equal Int.equal arr arr_sorted *)
