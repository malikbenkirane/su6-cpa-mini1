let rec uniq l = match l with
| x::r -> if List.mem x r then r else x::(uniq r)
| [] -> []

let kruskal vlist =
  let _, sorted_edges = Graph.sort_edges vlist in
  (* first pass : acm edges list *)
  let n, ufi, ufri, ufp, ufg = Unionfind.init vlist in
  let rec kruskal edges acm_edges = match edges with
    | ((Graph.Edge (v1, v2, _)) as e)::redges -> 
      let c = Unionfind.union
        (Hashtbl.find ufri v1) (Hashtbl.find ufri v2) ufp ufg in
      if c then kruskal redges acm_edges
      else kruskal redges (e::acm_edges)
    | [] -> acm_edges
  (* in kruskal sorted_edges [] uf *)
  in 
  let acm_edges = kruskal sorted_edges [] in
  (* acm_edges *)
  (* second pass : acm successors list *)
  let acm_successors = Hashtbl.create n in
  List.iter
    (fun e -> match e with
      Graph.Edge (v1, v2, _) -> Hashtbl.add acm_successors v1 v2;
                                Hashtbl.add acm_successors v2 v1;
    ) acm_edges;
  List.map
    (fun v -> let successors = uniq (Hashtbl.find_all acm_successors v) in
              (Graph.str_of_point v) ^ ":" ^ (Graph.str_of_points successors)
    ) vlist
