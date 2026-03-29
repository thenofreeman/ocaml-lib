let print_fizzbuzz n =
  let rec build_fizzbuzz n acc =
    match n with
    | 0 -> acc
    | n -> build_fizzbuzz (n-1) ((
      if n mod 5 = 0 && n mod 3 = 0 then "FizzBuzz"
      else if n mod 3 = 0 then "Fizz"
      else if n mod 5 = 0 then "Buzz"
      else Printf.sprintf "%d" n
    ) :: acc)
  in

  List.iter (Printf.printf "%s\n") (build_fizzbuzz 100 [])
;;

let () =
    print_fizzbuzz 100;;
