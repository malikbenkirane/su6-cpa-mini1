let print_points v = print_string (
    "[" ^ (Graph.str_of_points v) ^ "]\n\n"
)

let print_ds ds =
    let s = String.concat "  "
        (List.map (fun s -> "[" ^ (Graph.str_of_points s) ^ "]") ds)
    in print_string (s ^ "\n")

let rec find x ds = match ds with
    | s::rs ->
    begin
        match find_s x s with
        | None -> find x rs
        | Some(xs) -> xs
    end
    | [] -> failwith "Not Found"

and find_s x s =
    let rec find_s x s1 s2 = match s1 with
        | y::rs ->
            if y = x then Some(s1@s2)
            else (find_s x rs (y::s2))
        | [] -> None
    in find_s x s []

(* non optimized solution : quadratic complexity test *)
let rec same s1 s2 = match s1 with
| t::rs -> List.mem t s2 && same rs s2
| [] -> true

let union x1 x2 ds =
    let s1, s2 = find x1 ds, find x2 ds in
    (*
        optimization : works well when len(s1), len(s2) > 1
        it works because find moves only the element found       
    let ss1, ss2 =
        match s1, s2 with
        | r1::rs1, r2::rs2 -> (
            match rs1, rs2 with
            | [], [] -> [r1], [r2]
            | _, _ -> 
                List.filter (fun x -> x <> r2) rs1,
                List.filter (fun x -> x <> r1) rs2 
        )
        | r::rs, [] | [], r::rs -> [], []
        | [], [] -> failwith "Empty subset"
    in
    *)
    (*
    print_points s1;
    print_points s2;
    *)
    (*
    if ss1 = ss2 then (
        print_string "not union\t"; print_ds ds;
        true, ds
    )
    *)
    if same s1 s2 then (
        (*print_string "not union\t"; print_ds ds;*)
        true, ds
    )
    else
        let others = List.filter (fun s -> s <> s1 && s <> s2) ds in
        let ds = (s1@s2)::others
        in (*print_string "union!\t\t"; print_ds ds;*)
        false, ds


let rec unsafe_init_ds elems = match elems with
    | e::rl -> [e]::(unsafe_init_ds rl)
    | [] -> []
