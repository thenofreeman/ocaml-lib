let insertion_sort ~cmp arr =
  let arr = Array.copy arr in
  for i = 1 to (Array.length arr) - 1 do
    let key = arr.(i) in

    let rec loop j =
      if j < 0 || cmp key arr.(j) then j
      else (
        arr.(j + 1) <- arr.(j);
        loop (j - 1)
      )
    in

    let j = loop (i - 1) in

    arr.(j + 1) <- key
  done;
  arr
