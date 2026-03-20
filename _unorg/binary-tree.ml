
type 'a binary_tree =
  | Leaf
  | Node of 'a binary_tree * 'a * 'a binary_tree;;

let rec list_of_tree = function
  | Leaf -> []
  | Node (lb, x, rb) -> (list_of_tree lb) @ (r :: (list_of_tree rb));;

let rec insert x = function
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (lb, r rb) ->
     if x < r then Node (insert x lb, r, rb)
     else Node (lb, r, insert x rb);;

let rec tree_of_list = function
  | [] -> Leaf
  | hd :: tl -> insert h (tree_of_list t);;

let sort x = list_of_tree (tree_of_list x);;

let rec mem x = function
  | Leaf -> false
  | Node (lb, r, rb) ->
     if x = r then true
     else (mem x lb) or (mem x rb);;

let rec height t = function
  | Leaf -> 0
  | Node (lb, _, rb) -> 1 + (max (height lb) (height rb));;
