(* graph.ml *)

type 'a t = {
  mutable e: int;
  directed: bool;
  data: ('a, 'a list ) Hashtbl.t;
}

let create ?(directed=false) v_list =
  let tbl = Hashtbl.create 100 in
  List.iter (fun v -> Hashtbl.replace tbl v []) v_list;

  { directed;
    e = 0;
    data = tbl }

let mem g v =
  Hashtbl.mem g.data v

let nvertices g =
  Hashtbl.length g.data

let nedges g =
  g.e

let add_vertex g v =
  Hashtbl.replace g.data v []

let add_vertices g vl =
  List.iter (add_vertex g) vl

let add_edge g v1 v2 =
  match Hashtbl.(find_opt g.data v1, find_opt g.data v2) with
  | Some vl1, Some vl2 ->
    (if not (List.mem v2 vl1) then
      Hashtbl.replace g.data v1 (v2 :: vl1);
      g.e <- g.e + 1
    );

    if not g.directed then
      (if not (List.mem v1 vl2) then
        Hashtbl.replace g.data v2 (v1 :: vl2));
  | _ -> failwith (Printf.sprintf "No such edge in graph: %d" v1)

let add_edges g e_list =
  let vl1, vl2 = List.split e_list in
  List.iter2 (add_edge g) vl1 vl2

(* let bfs g = () *)
(* let dfs g = () *)

(* let of_matrix = () *)
(* let to_matrix g = () *)

(* let of_adj_list = () *)
(* let to_adj_list g = () *)

(* let of_list = () *)
(* let to_list g = () *)

let pp = ()
let pp_matrix = ()

let%test "Creating vertices" =
  let g = create [1;2;3] in
  add_edges g [(1,3); (2,1); (1,2)];

  nedges g = 3
