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

let test1 = 
  "tarjan" >:::
    [ (*
      ("cycle in itself" >:: fun () ->
        let g = G.create () in
        let v = "a" in
        G.add_vertex g v;
        G.add_edge g v v;
        let ans = T.find g in
        if ans = [["a"]]
        then print_endline "passed"
        else print_endline "failed"
       )
    ; ("two isolated islands" >:: fun () ->
        let g = G.create () in
        let v = "a" in
        G.add_vertex g v;
        let ans = T.find g in
        if ans = [["a"]]
        then print_endline "passed"
        else print_endline "failed"
       )
    ; ("bridge" >:: fun () ->
        let g = G.create () in
        G.add_vertex g "a";
        G.add_vertex g "b";
        G.add_edge g "a" "b";
        let ans = T.find g in
        if ans = [["a"];["b"]]
        then print_endline "passed"
        else begin 
          List.iter (fun xs ->
            printf "%s\n%!" (String.concat "," xs)
          ) ans;
          print_endline "failed"      
        end
    )
    ; ("simple cycle" >:: fun () ->
        let g = G.create () in
        G.add_vertex g "a";
        G.add_vertex g "b";
        G.add_edge g "a" "b";
        G.add_edge g "b" "a";
        let ans = T.find g in
        if ans = [["a";"b"]]
        then print_endline "passed"
        else begin 
          List.iter (fun xs ->
            printf "%s\n%!" (String.concat "," xs)
          ) ans;
          print_endline "failed"      
        end
    )
      *)
      ("lablqt" >:: fun () ->
      let open Graph in
      let module D = Dot_ast in
      let filename = "lablqt.qt5.dot" in
      let ast = 
        let buf = Lexing.from_channel (open_in filename) in
        (Dot_parser.file Dot_lexer.token buf).D.stmts
      in
      let g = G.create () in
      List.iter (function
        | Dot_ast.Node_stmt ((Dot_ast.String id,_),_) -> G.add_vertex g id
        | Dot_ast.Edge_stmt (x,xs,_) -> begin
            let f = function
              | D.NodeId (D.String s,_) -> s
              | _ -> assert false
            in
            let x = f x in
            let xs = List.map f xs in
            (*printf "%s, [%s]\n%!" x (String.concat "," xs) *)
            let _ = List.fold_left (fun acc x ->
              G.add_edge g acc x; x
            ) x xs in
            ()
        end
        | _ -> ()
      ) ast;
      let ans = T.find g in
      print_endline "EVALUATING ANSWER";
      List.iter (fun xs ->
        printf "%s\n%!" (String.concat "," xs)
      ) ans
      
    )
    ]

let _ = run_test_tt_main test1















