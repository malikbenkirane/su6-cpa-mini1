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

(* non optimized solution : quadratic complexity
let rec same s1 s2 = match s1 with
| t::rs -> List.mem t s2 && same rs s2
| [] -> true
*)

let union x1 x2 ds =
    let s1, s2 = find x1 ds, find x2 ds in
    (*
        optimization track : works well when len(s1), len(s2) > 1
        it works because find moves only the element found       
    *)
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
    if ss1 = ss2 then
        true, ds
    (*
    if same s1 s2 then
        true, ds
    *)
    else
        let others = List.filter (fun s -> s <> s1 && s <> s2) ds in
        false, (s1@s2)::others


let rec unsafe_init_ds elems = match elems with
    | e::rl -> [e]::(unsafe_init_ds rl)
    | [] -> []
