open Core
open Components

module PQueue = struct
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    let empty = Empty
    let rec push queue p e = match queue with
        | Empty -> Node (p, e, Empty, Empty)
        | Node (pc, ec, left, right) ->
            if p <= pc
                then Node(p, e, push right pc ec, left)
                else Node(pc, ec, push right p e, left)
    exception Queue_is_empty
    let rec remove_top = function
        | Empty -> raise Queue_is_empty
        | Node (p, e, left, Empty) -> left
        | Node (p, e, Empty, right) -> right
        | Node (p, e, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right)) ->
            if lp <= rp
                then Node (lp, le, remove_top left, right)
                else Node (rp, re, left, remove_top right)
    let pop = function
        | Empty -> raise Queue_is_empty
        | Node (p, e, _, _) as queue -> (p, e, remove_top queue)
end

module FunctionMap = Map.Make(String)
module ConstMap = Map.Make(String)

module Signature = struct
    exception Value_not_found
    let rec of_functions comps = match comps with
        | [] -> FunctionMap.empty
        | c :: cs -> let rest = of_functions cs in
            FunctionMap.add c.name c rest
    let rec of_constants vecs = match vecs with
        | [] -> ConstMap.empty
        | (Leaf c, v)::ps -> let rest = of_constants ps in
            ConstMap.add c v rest
        | _ -> failwith "what are you doing"
    let create_constant c k =
        (Leaf c.name, Array.make k (c.apply []))
    let rec add_inputs vecs s = match vecs with
        | [] -> s
        | (Leaf n, v):: vs -> let rest = add_inputs vs s in
            ConstMap.add n v rest
        | _ -> failwith "seriously, stop"
end

let select i l = List.map (fun x -> x.(i)) l

let rec eval p functions constants = match p with
    | Leaf c -> ConstMap.find c constants
    | Node (f, args) -> let c = (FunctionMap.find f functions) in
        let results = List.map (fun a -> eval a functions constants) args in
        Array.mapi (fun i _ -> c.apply (select i results)) (List.hd results)
