#load "graph.cmo";;
#load "uf.cmo";;
#load "read_points.cmo";;

open Graph
open Read_points
open Uf

let rec uniq l = match l with
| x::r -> if List.mem x r then r else x::(uniq r)
| [] -> []

let kruskal vlist =
    let _, sorted_edges = Graph.sort_edges vlist in
    (* first pass : acm edges' list *)
    let uf = Uf.unsafe_init_ds vlist in
    let rec kruskal edges acm_edges uf = match edges with
        | ((Graph.Edge (v1, v2, _)) as e)::redges -> 
            let c, uf = Uf.union v1 v2 uf in
                if c then kruskal redges acm_edges uf
                else kruskal redges (e::acm_edges) uf 
        | [] -> acm_edges
    (* in kruskal sorted_edges [] uf *)
    in 
    let acm_edges = kruskal sorted_edges [] uf in
    (* acm_edges *)
    (* second pass : acm successors list *)
    let acm_successors = Hashtbl.create (List.length vlist) in
    List.iter (fun e ->
            match e with Graph.Edge (v1, v2, _) ->
                Hashtbl.add acm_successors v1 v2;
                Hashtbl.add acm_successors v2 v1;
    ) acm_edges;
    List.map (
        fun v ->
            let successors = uniq (Hashtbl.find_all acm_successors v)
            in (Graph.str_of_point v)
                ^ ":" ^ (Graph.str_of_points successors)
    ) vlist

let read_vertices () =
    let vertices = ref [] in
        try 
        while true do
            let x, y =
                Scanf.bscanf Scanf.Scanning.stdin "%f %f\n" (fun x y -> (x, y))
            in vertices := (Vertex (x, y))::!vertices
        done ; !vertices
        with End_of_file -> !vertices
;;

#trace find

let _ =
    let vertices = read_vertices () in
    (* kruskal vertices *)
    print_string (String.concat "\n" (kruskal vertices));
    print_string "\n"
;;

Scanf.scanf "%d" (fun x -> x);;

