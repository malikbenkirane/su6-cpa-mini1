open Graph

let read_vertices () =
    let vertices = ref [] in
        try 
        while true do
            let x, y =
                Scanf.bscanf Scanf.Scanning.stdin "%f %f\n" (fun x y -> (x, y))
            in vertices := (Vertex (x, y))::!vertices
        done ; !vertices
        with End_of_file -> !vertices
