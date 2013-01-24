open Printf 
open OUnit

module G = struct
  include Graph.Imperative.Digraph.Concrete(struct
    include String
    let equal x y = (x=y)
    let hash s =
      (* adopted from Janestreet's Core *)
      let len = length s in
      if len = 0 then 0
      else if len > 30 then Hashtbl.hash_param 1 1 s
      else
        let res = ref (int_of_char (String.unsafe_get s 0)) in
        for i = 1 to len - 1 do
          res := !res * 19 + int_of_char (String.unsafe_get s i)
        done;
        !res land 0x3FFFFFFF
  end)
  let to_string (x:string) = x
end
module T = Tarjan.Make(G)

module G_out = struct
  include Graph.Imperative.Digraph.Concrete(struct 
    type t = [ `Single of string  | `Cycle of string list ]
    let equal x y = match (x,y) with
      | (`Cycle a,`Cycle b) -> a=b
      | (`Single a, `Single b) -> a=b
      | _ -> false 
    let hash = function
      | `Single s -> G.V.hash s * 2 + 1
      | `Cycle xs -> G.V.hash (List.hd xs)  * 2 
    let compare x y = match (x,y) with
      | (`Single _, `Cycle _)  -> -1
      | (`Cycle _, `Single _)  -> 1
      | (`Cycle a, `Cycle b)   -> compare a b
      | (`Single a, `Single b) -> compare a b
  end)
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let vertex_name (x: V.t)  = 
    let escape x = x in
    match x with
    | `Single s -> escape s
    | `Cycle xs -> escape (String.concat "," xs)
  let vertex_attributes _ = []
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let get_subgraph _ = None
  
end
module GraphPrinter = Graph.Graphviz.Dot(G_out)

module R = Reducer.Make(G)(G_out)

exception Not_equal
let compare_graphs g1 g2 = 
  try
    let f g1 g2 =
      G_out.iter_vertex (fun v -> if not (G_out.mem_vertex g2 v) then raise Not_equal) g1 in
    f g1 g2;
    f g2 g1;
    
    let f g1 g2 = 
      G_out.iter_edges (fun v1 v2 ->
        if not (G_out.mem_edge g2 v1 v2) then raise Not_equal
      ) g1
    in
    f g1 g2;
    f g2 g1;
    true
  with Not_equal -> false


let f_alone s = `Single s
let f_cycle xs = `Cycle xs
let test1 = 
  "????" >:::
    [ ("a1" >:: fun () ->      
        let g = G.create ~size:20 () in
        let v = "a" in
        G.add_vertex g v;
        G.add_edge g v v;
        let gout = R.rebuild g ~f_alone ~f_cycle in
        print_endline "";
        GraphPrinter.output_graph stdout gout;
        let expected = 
          let g = G_out.create () in
          let v = `Single "a" in
          G_out.add_vertex g v;
          G_out.add_edge g v v;
          g
        in
        if compare_graphs expected gout 
        then print_endline "\npassed"
        else print_endline "failed";
        ()
      );
      ("a2" >:: fun () ->      
        let g = G.create ~size:20 () in
        List.iter (G.add_vertex g) ["a";"b";"c"];
        G.add_edge g "a" "b";
        G.add_edge g "b" "c";
        G.add_edge g "c" "a";
        let gout = R.rebuild g ~f_alone ~f_cycle in
        print_endline "";
        GraphPrinter.output_graph stdout gout;
        let expected = 
          let g = G_out.create () in
          let v = `Cycle ["a";"b";"c"] in
          G_out.add_vertex g v;
          G_out.add_edge g v v;
          g
        in
        if compare_graphs expected gout 
        then print_endline "\npassed"
        else print_endline "failed"
      );
      ("a3" >:: fun () ->      
        let g = G.create ~size:20 () in
        List.iter (G.add_vertex g) ["a";"b";"c";"d"];
        List.iter (fun (x,y) -> G.add_edge g x y) [("a", "b"); ("b","a"); ("b","c"); ("c","d"); ("d","c")];
        let gout = R.rebuild g ~f_alone ~f_cycle in
        print_endline "";
        GraphPrinter.output_graph stdout gout;
        let expected = 
          let g = G_out.create () in
          let v1 = `Cycle ["a";"b"] in
          let v2 = `Cycle ["d";"c"] in
          List.iter (G_out.add_vertex g) [v1;v2];
          List.iter (fun (x,y) -> G_out.add_edge g x y) [(v1,v1);(v1,v2);(v2,v2)];
          g
        in
        if compare_graphs expected gout 
        then print_endline "\npassed"
        else print_endline "failed"
      )

    ]

let _ = OUnit.run_test_tt_main test1
