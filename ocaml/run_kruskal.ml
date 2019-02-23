open Read_points
let _ =
    let vertices = read_vertices () in
    (* kruskal vertices *)
    print_string (String.concat "\n" (kruskal vertices));
    print_string "\n"
