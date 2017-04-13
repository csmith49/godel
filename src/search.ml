open Core
open Components
open Tasks
open Eval
open Config
open Normal

(* let's be careful with the hvalue construction *)
module TD = struct
    type t = int * int
    let leq a b = match a, b with
        | (lh, lv), (rh, rv) ->
            lh <= rh || (lh = rh && lv <= rv)
end
(* so now the hvalue is paired *)
let hvalue (x, _) = match !Config.expansion_metric with
    | "size" -> (program_size x, Hashtbl.hash x)
    | _ -> failwith "Unrecognized heuristic!"
(* and our frontier is as follows *)
module Frontier = Eval.PriorityQueue(TD)

(* SECTION FOR NORMALIZATION STUFF *)
(* we might want to cache things *)
module ProgramTable = Hashtbl.Make(Program)
let tbl : bool ProgramTable.t ref = ref (ProgramTable.create 139)

(* useful wrappers *)
let stats_wrapper (f : Program.t -> bool) : (Program.t -> bool) =
    fun p ->
        let t = Sys.time () in
        let ans = f p in begin
            Config.normalize_time := !Config.normalize_time +. (Sys.time ()) -. t;
            ans
        end
let noisy_wrapper (f : Program.t -> bool) : (Program.t -> bool) =
    fun p ->
        let ans = f p in
            if not ans then print_endline ("---> NORMALIZED: " ^ program_string p);
            ans
(* the different root normals we can use *)
let dtree_rn (p : Program.t) : bool =
    not (Normal.DTree.match_program p !Config.dtree)
let system_rn (p : Program.t) : bool =
    not (Normal.System.match_program p !Config.system)
(* which we select early based on config *)
let root_normal : (Program.t -> bool) =
    let rn = if !Config.use_dtree then dtree_rn else system_rn in
    let rnp = if !Config.stats then stats_wrapper rn else rn in
    if !Config.noisy then noisy_wrapper rnp else rnp
(* and then repeat with normal *)
let rec dtree_n (p : Program.t) : bool = match p with
    | Leaf x -> not (Normal.DTree.match_program p !Config.dtree)
    | Node (f, ss) ->
        not (Normal.DTree.match_program p !Config.dtree) && (List.for_all dtree_n ss)
let rec system_n (p : Program.t) : bool = match p with
    | Leaf x -> not (Normal.System.match_program p !Config.system)
    | Node (f, ss) ->
        not (Normal.System.match_program p !Config.system) && (List.for_all system_n ss)
let normal : (Program.t -> bool) =
    let n = if !Config.use_dtree then dtree_n else system_n in
    let np = if !Config.stats then stats_wrapper n else n in
    if !Config.noisy then noisy_wrapper np else np

(* used to short-circuit computation *)
exception Success of Vector.t

let get_type = function
    | VList _ -> TList
    | VInt _ -> TInt
    | VBool _ -> TBool
    | VTree _ -> TTree
    | VString _ -> TString
    | VError -> failwith "error on input"
    | VDontCare -> failwith "underspecified input"

type prod_rule = {
    r_domain : typ list;
    r_codomain : typ;
    r_name : string
}

let comp_to_rule c = {
    r_domain = c.domain;
    r_codomain = c.codomain;
    r_name = c.name
}

let input_to_rule v = {
    r_domain = [];
    r_codomain = get_type (snd v).(0);
    r_name = match (fst v) with
        | Leaf c -> c
        | _ -> failwith "not well-defined input"
}

let to_function functions constants labels program varrays =
    let constants = Signature.add_inputs (List.combine labels varrays) constants in
    eval program functions constants

let ac_check task prog x y z =
    let vector_size = Array.length (snd (List.hd task.inputs)) in
    (* build the signature, sans inputs *)
    let constants, functions =
        List.partition (fun c -> (List.length c.domain) = 0) task.components
    in let constants =
        List.map (fun c -> Signature.create_constant c vector_size) constants
    in let constants = Signature.of_constants constants
    in let functions = Signature.of_functions functions
    in let labels = List.map fst task.inputs
    (* execute wraps the rest of the construction - just pass in varrays *)
    in let f = to_function functions constants labels prog
    in
    let comm_check a b =
        try (f [a; b] = f [b; a])
        with Not_found -> false
    in
    let assoc_check a b c =
        try (f [f [a; b]; c] = f [a; f [b; c]])
        with Not_found -> false
    in (comm_check x y) && (assoc_check x y z)

let solveTD task =
    let vector_size = Array.length (snd (List.hd task.inputs)) in
    (* construct the signature to search over *)
    let input_rules = List.map input_to_rule task.inputs in
    let component_rules = List.map comp_to_rule task.components in
    let rules = input_rules @ component_rules in
    (* used to define correctness *)
    let final_goal = snd (apply_component task.target task.inputs) in
    (* constructing signature for evaluation *)
    let constants, functions =
        List.partition (fun c -> (List.length c.domain) = 0) task.components
    in let constants =
        List.map (fun c -> Signature.create_constant c vector_size) constants
    in let constants = Signature.of_constants (constants @ task.inputs)
    in let functions = Signature.of_functions functions
    in
    (* methods for keeping track of unique holes *)
    let hole_count = ref 0 in
    let is_hole s =
        try (String.sub s 0 4) = "HOLE"
        with Invalid_argument _ -> false
    in
    let fresh_hole _ =
        hole_count := !hole_count + 1;
        (Leaf ("HOLE" ^ (string_of_int !hole_count)))
    in
    (* functions for expanding programs *)
    let get_all_comps t = List.filter (fun x -> x.r_codomain = t) rules in
    let unexpanded_comp c = match c.r_domain with
        | [] -> Leaf c.r_name
        | xs -> Node (c.r_name, List.map (fun x -> fresh_hole x) xs)
    in
    let rec fill_hole prog hole filling = match prog with
        | Node (f, args) -> Node (f, List.map (fun a -> fill_hole a hole filling) args)
        | Leaf c -> if c = hole then filling else (Leaf c)
    in
    let get_type c = (List.find (fun x -> c = x.r_name) rules).r_domain in
    let get_holes prog =
        let rec get_holes_aux prog t = match prog with
            | Node (f, args) ->
                let nt = List.combine args (get_type f) in
                List.flatten (List.map (fun (x, y) -> get_holes_aux x y) nt)
            | Leaf c -> if (is_hole c) then [(c, t)] else []
        in match prog with
            | Node (f, args) ->
                let nt = List.combine args (get_type f) in
                List.flatten (List.map (fun (x, y) -> get_holes_aux x y) nt)
            | _ -> []
    in
    let get_all_completions prog hole =
        let (h, t) = hole in
        let cs = List.map unexpanded_comp (get_all_comps t) in
        List.map (fun x -> fill_hole prog h x) cs
    in
    let all_expansions prog = match get_holes prog with
        | [] -> []
        | h :: hs -> get_all_completions prog h
    in
    (* now the iterative part - we search over a pqueue *)
    let frontier = ref Frontier.empty in
    let h_seen = ref 0 in
    let metric p = hvalue (p, [||]) in
    (* TODO - add normalization here *)
    let matches_goal p =
        try (eval p functions constants) = final_goal
        with Not_found -> false
    in
    let check_program p =
        if (matches_goal p) && (not !Config.enumerate)
            then raise (Success (p, [||]));
        if (!Config.reduce) && not (normal p) then false
        else true
    in
    let add_program p =
        if (check_program p) then
            frontier := Frontier.push !frontier (metric p) p;
    in
    begin
        List.iter (fun c -> let p = (unexpanded_comp c) in
            frontier := (Frontier.push !frontier (metric p) p);
        ) (get_all_comps task.target.codomain);

        while !h_seen < !Config.max_height; do
            let (h, _), p, q = Frontier.pop !frontier in
            frontier := q;
            program_count := !program_count + 1;
            if !noisy then print_endline (program_string p);
            if h > !h_seen then h_seen := h;
            List.iter add_program (all_expansions p);
        done;
    end

let solveBU task =
    let seen = ref VMSet.empty in
    let vector_size = Array.length (snd (List.hd task.inputs)) in
    let components = task.rec_components @ task.components in
    let final_goal = snd (apply_component task.target task.inputs) in

    let int_array = Array.make !Config.max_height VMSet.empty in
    let bool_array = Array.make !Config.max_height VMSet.empty in
    let list_array = Array.make !Config.max_height VMSet.empty in
    let tree_array = Array.make !Config.max_height VMSet.empty in
    let string_array = Array.make !Config.max_height VMSet.empty in

    (* redefine apply_comp to avoid self on input vec *)
    let apply_component c args =
        let f i =
            let arg = List.map (fun a -> (snd a).(i)) args in
            let input = List.map (fun a -> (snd a).(i)) task.inputs in
            if vlist_lt arg input then
                c.apply arg
            else
                VError
        in if List.exists (fun x -> x.name = c.name) task.rec_components then
            ( (Node (c.name, List.map fst args)), Array.init vector_size f)
        else
            apply_component c args
    in

    (* are we done? do we prune? *)
    let check_vector ?(init=false) v =
        if !symmetry_reduction && (not init) && VMSet.seen (snd v) (!seen)
        then false
        else if !Config.reduce && (not (root_normal (fst v)))
        then false
        else begin
            if !noisy then begin
                print_endline ("---> PROGRAM constructed at # " ^ (string_of_int !program_count));
                print_endline (program_string (fst v) ^ " -- " ^ varray_string (snd v));
            end;
            incr program_count;
            if !symmetry_reduction && (not init) then
                seen := VMSet.add v (!seen);
            if (final_goal = (snd v)) && (not !enumerate) then
                raise (Success v);
            true
        end
    in

    let filt t = (fun c -> c.codomain = t) in
    let int_components = List.filter (filt TInt) components in
    let bool_components = List.filter (filt TBool) components in
    let list_components = List.filter (filt TList) components in
    let tree_components = List.filter (filt TTree) components in
    let string_components = List.filter (filt TString) components
    in

    let apply_comp f types i =
        let rec apply_cells types acc locations = match types, locations with
            | (typ::typs, i::locs) ->
                VMSet.iter (fun x -> apply_cells typs (x::acc) locs) begin
                    match typ with
                        | TInt -> int_array.(i)
                        | TBool -> bool_array.(i)
                        | TList -> list_array.(i)
                        | TTree -> tree_array.(i)
                        | TString -> string_array.(i)
                end
            | ([], []) -> f (List.rev acc)
            | _ -> failwith "impossible!"
        in divide (apply_cells types []) (List.length types) (i - 1) []
    (* apply component to as many values as you can *)
    in let expand_component c array i =
        let f x =
            let vector = apply_component c x in
            let (h_value, _) = hvalue vector in
                if h_value < !Config.max_height then
                    if check_vector vector then
                        array.(h_value) <- VMSet.add vector array.(h_value)
        in apply_comp f c.domain i
    (* given a bunch of seen programs and components, make new programs *)
    in let expand_type (mat, components) i =
        List.iter (fun c -> expand_component c mat i) components
    (* expand out each type as much as possible *)
    in let expand i =
        List.iter (fun x -> expand_type x i)
        [
            (int_array, int_components);
            (bool_array, bool_components);
            (list_array, list_components);
            (tree_array, tree_components);
            (string_array, string_components);
        ]
    in
    (* need to pull out constants and prop array of each type *)
    List.iter
        (fun c -> if (List.length c.domain) = 0
            then let v =
                (Leaf c.name, Array.make vector_size (c.apply [])) in
                let array = match c.codomain with
                    | TList -> list_array
                    | TBool -> bool_array
                    | TInt -> int_array
                    | TString -> string_array
                    | TTree -> tree_array
                in array.(1) <- VMSet.add v array.(1);
        )
    components;

    List.iter
        (fun input -> let array = match (snd input).(1) with
            | VList _ -> list_array
            | VInt _ -> int_array
            | VBool _ -> bool_array
            | VTree _ -> tree_array
            | VString _ -> string_array
            | VError -> failwith "error on input"
            | VDontCare -> failwith "underspecified input"
            in array.(1) <- VMSet.add input array.(1)
        )
        task.inputs;
    if !noisy then
        print_endline ("Goal: " ^ (varray_string final_goal));
    begin
        let initialize array =
            VMSet.filter (fun a -> check_vector ~init:true a) array
        in
        list_array.(1) <- initialize list_array.(1);
        int_array.(1) <- initialize int_array.(1);
        bool_array.(1) <- initialize bool_array.(1);
        tree_array.(1) <- initialize tree_array.(1);
        string_array.(1) <- initialize string_array.(1);
    end;
    for i = 2 to !Config.max_height - 1; do
        let prevcount = !program_count in

        if !noisy then begin
            print_endline ("---> DEPTH " ^ (string_of_int i) ^ " -> " ^ (string_of_int (!program_count - prevcount)));
        end;
        expand i;
    done

let strategies = [
    ("bu", solveBU);
    ("td", solveTD)
]

(* construct task and start synthesis *)
let synthesize target =
    let task =
        try List.find (fun x -> x.target.name = target) synthesis_targets
        with Not_found ->
            print_endline ("Not found. Available tasks:");
            List.iter (fun task -> print_endline task.target.name) synthesis_targets;
            raise Not_found
    in let components = match task.target.codomain with
        | TInt -> cond_int::task.components
        | TBool -> cond_bool::task.components
        | TList -> cond_list::task.components
        | TTree -> cond_tree::task.components
        | TString -> cond_string::task.components
    in begin
        let solve = List.assoc !Config.expansion_strategy strategies in
        try solve {task with components = components}
        with (Success v) -> begin
            print_endline (program_string (fst v));
        end
    end
