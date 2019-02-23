type point = Vertex of (float * float)
(* e.g. (x1, x2) *)

type edge = Edge of (point * point * float)
(* e.g. (p1, p2, distance) *)

let zero = Vertex (0., 0.)

let str_of_point v =
	match v with Vertex (x, y) ->
	(string_of_int (int_of_float x)) ^ ","
	^ (string_of_int (int_of_float y))

let str_of_points vlist =
	String.concat ";" (List.map str_of_point vlist)

(* eucd: euclidian distance *)
let eucd p1 p2 =
    match p1, p2 with
    (Vertex (x1, y1), Vertex (x2, y2)) ->
    let dx, dy = (x1 -. x2), (y1 -. y2)
    in sqrt (dx *. dx +. dy *. dy)

(* comp: compare two edges *)
let comp e1 e2 =
    match e1, e2 with
    (Edge (_, _, d1)), (Edge (_, _, d2)) -> compare d1 d2

(* complete_graph: point list -> int * (point * point) list *)
let rec complete_graph vlist =
    match vlist with
    | [] -> (0, [])
    | v::rv ->
        let ne, vertices = edges_of v rv
        in let nr, rvertices = complete_graph rv
        in (ne + nr, vertices @ rvertices)

(* edges_of: point -> point list -> int * (point * point) list *)
and edges_of v vlist =
    match vlist with
    | [] -> (0, [])
    | v1::rv ->
        let ne, rvertices = edges_of v rv
        in (ne+1, (Edge (v, v1, eucd v v1))::rvertices)

(* sort_edges: (point * point) list -> (point * point) list *)
let sort_edges vlist =
    let n, edges = complete_graph vlist
    in n, List.sort comp edges
