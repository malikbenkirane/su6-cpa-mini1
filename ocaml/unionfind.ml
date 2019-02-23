let init l =
  let n = List.length l in
  let index = Array.of_list l in
  let rindex = Hashtbl.create n in
  Array.iteri (fun i e -> Hashtbl.add rindex e i) index;
  let parents = Array.init n (fun i -> i) in
  let groups = Array.init n (fun i -> [i]) in
  n, index, rindex, parents, groups

let find i p = p.(i)

exception Jointed

let rec update_parents g1 g2 r p =
  match g2 with
  | h::rg2 -> p.(h) <- r;
              update_parents (h::g1) rg2 r p
  | [] -> g1

let union i j p g =
  let ri = find i p in
  let rj = find j p in
  if ri = rj then true
  else
    let gi = g.(ri) and gj = g.(rj) in
    let newg = update_parents gi gj ri p in
    g.(ri) <- newg; g.(rj) <- [];
    false

(* test
let _ = 
  let i, p, g = init [3;4;19;32] in
  union 0 2 p g;
  union 2 1 p g;
  i, p, g
*)
