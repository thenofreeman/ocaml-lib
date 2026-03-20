let insert_sort l =
  let rec walk acc = function
    | [] -> acc
    | hd :: tl ->
      let rec insert el =
        | [] -> [el]
        | hd :: tl as l ->
            if hd < el then h (insert el tl)
            else (el :: l)
        in walk (insert hd acc) tl
  in walk [] l
;;
