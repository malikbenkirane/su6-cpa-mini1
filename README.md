compilation command (with debug flag) :
```
ocamlc -g graph.ml read_points.ml uf.ml kruskal.ml
```

debug
```
OCAMLRUNPARAM=b ./a.out < input.points
```

test failed with `Stack_overflow` exception
