(* 
 * http://algowiki.net/wiki/index.php?title=Tarjan%27s_algorithm
 *)
open Printf
open Graph

exception Found
module Stack = struct
  include Stack
    
  let mem what st = 
    try 
      iter (fun x -> if x=what then raise Found) st;
      false
    with
        Found -> true
end

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit 
  val succ : t -> V.t -> V.t list
end

module Make(G:G) = struct
  module H = Hashtbl.Make(G.V)
  module S = Stack

  let find g = 
    let index = ref 0 in
    let indexes = H.create 999 in
    let lowlinks = H.create 999 in
    let s = S.create () in
    
    let ans = ref [] in

    let rec tarjan v = 
      H.add indexes v !index;
      H.add lowlinks v !index;
      incr index;(*
      let info v =
        printf "tarjan of %s: index = %d, lowlink=%d\n%!" 
          (G.to_string v) (H.find indexes v)(H.find lowlinks v)
      in 
      info v;*)
      let succs = G.succ g v in
      S.push v s;
      (*if succs = [] then () else *) begin
        List.iter (fun v' ->
          (*print_endline "iter of succ";*)
          if not (H.mem indexes v') then (
            tarjan v';
            H.add lowlinks v (min (H.find lowlinks v) (H.find lowlinks v'));
          ) else 
            if Stack.mem v' s then begin
              H.add lowlinks v (min (H.find lowlinks v) (H.find indexes v'));
            end
        ) succs;
        
        if H.find lowlinks v = H.find indexes v 
        then begin
          let rec loop acc = 
            let v' = S.pop s in
            if v = v' then v'::acc
            else loop (v'::acc)
          in
          ans := (loop []) :: !ans
        end
      end
    in
    G.iter_vertex (fun v ->   
      if not (H.mem indexes v) then
        tarjan v
    ) g;
    !ans
end












