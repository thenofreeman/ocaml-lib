(* let linear arr = () *)
(* let binary arr = () *)

let find_min ?(cmp=( < )) l =
  let rec walk min = function
    | [] -> min
    | hd :: tl -> if cmp hd min then walk hd tl
                  else walk min tl
  in
  match l with
    | hd :: tl -> Some (walk hd tl)
    | _ -> None
;;

let find_max ?(cmp=( > )) l =
  find_min ~cmp l

let rec is_sorted ?(cmp=( >= )) l =
  match l with
  | [] -> true
  | hd :: tl -> List.for_all (fun x -> cmp x hd) tl && is_sorted tl
;;

let is_permutation ?(cmp=Int.compare) l1 l2 =
  if (List.compare_lengths l1 l2) <> 0 then false
  else begin
    let l1 = List.fast_sort cmp l1 in
    let l2 = List.fast_sort cmp l2 in
    List.equal (fun a b -> a = b) l1 l2
  end
;;
