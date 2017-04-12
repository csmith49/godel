open Sexplib

(* utility functions for printing *)
let rec join (xs : string list) (sep : string) : string = match xs with
    | [] -> ""
    | s :: [] -> s
    | s :: ss -> s ^ sep ^ (join ss sep)
let rec repeat_string (s : string) (k : int) : string =
    if k <= 0 then
        ""
    else
        s ^ (repeat_string s (k - 1))

(* and some utility functions for option types *)
let liftOM f a b = match a, b with
    | Some a, Some b -> f a b
    | _ -> None

(* and utility for random subsets (taken from 99 ocaml problems) *)
let rec random_selection list n =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n-1) (picked :: acc) rest (len-1)
    in
    let len = List.length list in
    aux (min n len) [] list len

(* strangely (?) we define subs before patterns *)
module StringMap = Map.Make(String)
type substitution = Core.program StringMap.t

(* we'll check consistency with subs often in matching *)
let can_add (k : string) (v : Core.program) (s : substitution) : bool =
    try
        (StringMap.find k s) = v
    with Not_found -> true
(* and combine subs carefully *)
let merge_sub (s : substitution) (t : substitution) : substitution option =
    let f sub (k, v) = match sub with
        | Some sub ->
            if (StringMap.mem k sub) && (StringMap.find k sub) <> v then
                None
            else Some (StringMap.add k v sub)
        | None -> None
    in List.fold_left f (Some s) (StringMap.bindings t)

(* which operate over patterns *)
module Pattern = struct
    type t =
        | PFunc of string * t list
        | PConst of string
        | PVar of string
    (* string manipulation stuff *)
    let rec of_sexp (s : Sexp.t) : t =
        let iv = fun c -> c.[0] = '@' in match s with
            | Sexp.Atom s -> if iv s then (PVar s) else (PConst s)
            | Sexp.List (x :: xs) -> PFunc (Sexp.to_string x, List.map of_sexp xs)
            | Sexp.List [] -> PConst "[]"
    let rec to_string (p : t) : string = match p with
        | PVar v
        | PConst v -> v
        | PFunc (f, xs) ->
            let join = fun a b -> a ^ " " ^ b in
            let kids = List.map to_string xs in
                "(" ^ f ^ (List.fold_left join "" kids) ^ ")"
    let of_string (s : string) : t = of_sexp (Sexp.of_string s)
    (* actually converting patterns to programs using subs *)
    let rec apply_sub (p : t) (s : substitution) : Core.program = match p with
        | PFunc (f, ss) -> Core.Node (f, List.map (fun a -> apply_sub a s) ss)
        | PConst c -> Core.Leaf c
        | PVar x -> try StringMap.find x s with Not_found -> Core.Leaf x
    (* and maybe we care about matching programs *)
    let rec match_program (p : Core.program) (t : t) : substitution option  = match p, t with
        | t, PVar x -> Some (StringMap.singleton x t)
        | Core.Node (f, arfs), PFunc (g, args) ->
            if g = f then
                let subproblems = List.map2 match_program arfs args in
                let f = liftOM merge_sub in
                let acc = Some StringMap.empty in
                    List.fold_left f acc subproblems
            else None
        | Core.Leaf c, PConst s -> if s = c then Some StringMap.empty else None
        | Core.Leaf c, PFunc (f, []) -> if f = c then Some StringMap.empty else None
        | _ -> None
    (* and sometimes we convert patterns to other patterns by being careful *)
    type p_sub = t StringMap.t
    let rec apply_p_sub (p : t) (s : p_sub) : t = match p with
        | PFunc (f, ss) -> PFunc (f, List.map  (fun a -> apply_p_sub a s) ss)
        | PConst c -> PConst c
        | PVar x -> try StringMap.find x s with Not_found -> PVar x
end

(* which we convert to flat patterns *)
module FlatPattern = struct
    (* nodes maintain some typing information *)
    type node =
        | NFunc of string
        | NConst of string
        | NVar of string
    (* but we really are constructing lists of them *)
    type t = node list
    (* the bindings help us normalize while we flatten *)
    type binding = node StringMap.t
    (* which we do very simply *)
    let rec of_pattern (p : Pattern.t) : t =
        let ss, _ = flatten p StringMap.empty in
            ss
    and flatten (p : Pattern.t) (b : binding) : t * binding = match p with
        | Pattern.PFunc (f, ss) ->
            let r = (fun (tt, b) p ->
                let tt_p, b_p = flatten p b in
                    tt @ tt_p, StringMap.union (fun k x y -> Some x) b b_p) in
            List.fold_left r ([NFunc f], b) ss
        | Pattern.PConst c -> [NConst c], b
        | Pattern.PVar x ->
            try [StringMap.find x b], b
            with Not_found ->
                let i = (StringMap.cardinal b) + 1 in
                let x_p = NVar ("*_" ^ (string_of_int i)) in
                    [x_p], StringMap.add x x_p b
    let binding_to_p_sub (b : binding) : Pattern.p_sub =
        let f = fun n -> match n with
            | NVar x -> Pattern.PVar x
            | _ -> failwith "nope" in
        StringMap.map f b
    let convert_pair (lhs : Pattern.t) (rhs : Pattern.t) : t * Pattern.t * Pattern.t =
        let fp, s = flatten lhs StringMap.empty in
        let ps = binding_to_p_sub s in
        fp, (Pattern.apply_p_sub lhs ps), (Pattern.apply_p_sub rhs ps)
end

(* utilities for loading files *)
let vlist_of_sexp xs =
    let paired = function
        | Sexp.List ((Sexp.Atom n) :: (Sexp.Atom v) :: []) ->
            (n, (int_of_string v))
        | _ -> failwith "not a valid pair"
    in List.map paired xs

let pair_of_sexp = function
    | Sexp.List (l :: [r]) -> (Pattern.of_sexp l, Pattern.of_sexp r)
    | _ -> failwith "not a valid rule"

(* so that we can construct the dtrees *)
module DTree = struct
    (* some types that will help our book-keeping *)
    type label = S | F | V
    type check = substitution -> bool
    (* the default check is really straightforward *)
    let true_check = fun s -> true
    (* and our eventual type is just a tree of these tagged values *)
    type t =
        | Internal of (t StringMap.t) * (t StringMap.t)
        | Terminal of check
    (* left keeps function symbols, right keeps variable symbols *)
    let empty : t = Internal (StringMap.empty, StringMap.empty)
    (* insertion is just like tries, but you have to be careful about in-place mods and funs vs vars *)
    (* so we start by converting flatpatterns to tries *)
    let rec of_flatpattern (fp : FlatPattern.t) (f : check) : t = match fp with
        | [] -> Terminal f
        | n :: ns ->
            let subtrie = of_flatpattern ns f in
            let fm, vm = match n with
                | FlatPattern.NFunc f -> StringMap.singleton f subtrie, StringMap.empty
                | FlatPattern.NConst c -> StringMap.singleton c subtrie, StringMap.empty
                | FlatPattern.NVar x -> StringMap.empty, StringMap.singleton x subtrie
            in Internal (fm, vm)
    (* which we then merge *)
    let rec merge (a : t) (b : t) : t = match a, b with
        (* possible different conditions merge with or - don't think these can even happen *)
        | Terminal f, Terminal g ->
            let h = fun s -> (f s) || (g s) in
                Terminal h
        (* simple recursion using maps union *)
        | Internal (lfm, lvm), Internal (rfm, rvm) ->
            let m = fun k x y -> Some (merge x y) in
            let fm = StringMap.union m lfm rfm in
            let vm = StringMap.union m lvm rvm in
                Internal (fm, vm)
        (* shouldn't compare internals with terminals, due to arity conditions *)
        | _, _ -> failwith "can't merge tries of different types"
    (* of course, we usually make from rules instead *)
    let of_rule (lhs : Pattern.t) (rhs : Pattern.t) (ordered : bool) : t =
        let f =
            (fun l r -> let fp, _, _ = FlatPattern.convert_pair l r in
                of_flatpattern fp true_check)
        in
        let g =
            (fun l r -> let fp, lhs, rhs = FlatPattern.convert_pair l r in
                let check = fun s -> (
                    let l = Pattern.apply_sub lhs s in
                    let r = Pattern.apply_sub rhs s in
                    let ans = Kbo.gt l r in
                        ans) in
                    of_flatpattern fp check)
        in
        match ordered with
            | false -> f lhs rhs
            | true -> merge (g lhs rhs) (g rhs lhs)
    (* checks are a little convoluted, because we have to jump laterally across the prog when we match *)
    (* so we convert programs into preorder programs *)
    module Preorder = struct
        type t = Core.program list
        let of_program (p : Core.program) : t = [p]
        let current_program (p : t) : Core.program = List.hd p
        let current_symbol (p : t) : string = match current_program p with
            | Core.Leaf c -> c
            | Core.Node (f, _) -> f
        let continue (p : t) : t = match p with
            | [] -> failwith "nowhere to go"
            | Core.Node (f, ps) :: xs -> ps @ xs
            | Core.Leaf c :: xs -> xs
        let skip (p : t) : t = match p with
            | [] -> failwith "nowhere to go"
            | _ :: xs -> xs
    end
    (* when matching, we assume no variables are in the program *)
    let rec match_preorder (ps : Preorder.t) (s : substitution) (dtree : t) : bool = match dtree with
        | Terminal f -> f s
        | Internal (fm, vm) ->
            (* do we match the current symbol? if so, continue down preorder *)
            let f_path =
                try let subtree = StringMap.find (Preorder.current_symbol ps) fm in
                    [(Preorder.continue ps, s, subtree)]
                with Not_found -> []
            (* for every consistent variable binding, skip the subterm and continue *)
            in
            let p = Preorder.current_program ps in
            let f = fun k v -> can_add k p s in
            let consistent = StringMap.bindings (StringMap.filter f vm) in
            let g = fun (k, v) -> Preorder.skip ps, StringMap.add k p s, v in
            let v_paths = List.map g consistent in
            (* then put the problems together and solve with short circuiting *)
            let subprobs = f_path @ v_paths in
                List.exists (fun (a, b, c) -> match_preorder a b c) subprobs
    and match_program (p : Core.program) (dtree : t) : bool =
        let ps = Preorder.of_program p in
        let s = StringMap.empty in
            match_preorder ps s dtree
    (* so we can visually verify what we're doing *)
    let rec to_string (dtree : t) : string =
        "start\n" ^ (to_string_aux dtree 0)
    and to_string_aux (dtree : t) (depth : int) : string =
        let vbar = "|  " in
        let start = "+--+ " in
        match dtree with
            | Terminal f ->
                let fs = if f (StringMap.empty) then "T" else "?" in
                (repeat_string vbar depth) ^ start ^ fs
            | Internal (fm, vm) ->
                let bindings = StringMap.bindings (StringMap.union (fun k x y -> Some x) fm vm) in
                let f = fun (k, v) ->
                    let s = (repeat_string vbar depth) ^ start ^ k in
                    let ks = to_string_aux v (depth + 1) in
                        if ks = "" then
                            s
                        else
                            s ^ "\n" ^ ks
                in join (List.map f bindings) "\n"
    let of_file (filename : string) (use_kbo : bool) : t =
        let dtree = ref empty in
        let parse_command s = match s with
            | Sexp.List ((Sexp.Atom "rules") :: xs) ->
                let pairs = List.map pair_of_sexp xs in
                let dtrees = List.map (fun (lhs, rhs) -> of_rule lhs rhs false) pairs in
                let m = fun dt -> dtree := merge !dtree dt in
                    List.iter m dtrees
            | Sexp.List ((Sexp.Atom "eqs") :: xs) ->
                if use_kbo then
                    let pairs = List.map pair_of_sexp xs in
                    let dtrees = List.map (fun (lhs, rhs) -> of_rule lhs rhs true) pairs in
                    let m = fun dt -> dtree := merge !dtree dt in
                        List.iter m dtrees
                else ()
            | _ -> ()
        in begin match (Sexp.load_sexp filename) with
            | Sexp.List xs -> List.iter parse_command xs
            | _ -> failwith "not a valid file"
        end;
        !dtree
end

module System = struct
    type pair = Pattern.t * Pattern.t
    type t = {
        rules : pair list;
        eqs : pair list;
    }
    let empty = {
        rules = [];
        eqs = [];
    }
    let merge (s : t) (t : t) : t =
        {
            rules = s.rules @ t.rules;
            eqs = s.eqs @ t.eqs;
        }
    let of_file (filename : string) (use_kbo : bool) : t =
        let system = ref empty in
        let parse_command s = match s with
            | Sexp.List ((Sexp.Atom "rules") :: xs) ->
                let pairs = List.map pair_of_sexp xs in
                let m = fun (lhs, rhs) -> system := {!system with rules = !system.rules @ [(lhs, rhs)]} in
                    List.iter m pairs
            | Sexp.List ((Sexp.Atom "eqs") :: xs) ->
                if use_kbo then
                    let pairs = List.map pair_of_sexp xs in
                    let m = fun (lhs, rhs) -> system := {!system with eqs = !system.eqs @ [(lhs, rhs)] @ [(rhs, lhs)]} in
                        List.iter m pairs
                else ()
            | _ -> ()
        in begin match (Sexp.load_sexp filename) with
            | Sexp.List xs -> List.iter parse_command xs
            | _ -> failwith "not a valid file"
        end;
        !system
    let match_program (p : Core.program) (s : t) : bool =
        let f (lhs, rhs) = match Pattern.match_program p lhs with
            | Some _ -> true
            | None -> false in
        let g (lhs, rhs) = match Pattern.match_program p lhs with
            | Some s -> Kbo.gt (Pattern.apply_sub lhs s) (Pattern.apply_sub rhs s)
            | None -> false
        in
            (List.exists f s.rules) || (List.exists g s.eqs)
    (* to make things easier, we convert our system to a dtree  *)
    let to_dtree (s : t) : DTree.t =
        let dtree = ref DTree.empty in
        let m = fun dt -> dtree := DTree.merge !dtree dt in
        let r_trees = List.map (fun (lhs, rhs) -> DTree.of_rule lhs rhs false) s.rules in
        let e_trees = List.map (fun (lhs, rhs) -> DTree.of_rule lhs rhs true) s.eqs in
            List.iter m (r_trees @ e_trees); !dtree
    let random_subset (s : t) (size : int) : t =
        let rs = List.map (fun r -> (r, false)) s.rules in
        let es = List.map (fun r -> (r, true)) s.eqs in
        let subset = random_selection (rs @ es) size in
        {
            rules = List.map fst (List.filter (fun (_, b) -> not b) subset);
            eqs = List.map fst (List.filter (fun (_, b) -> b) subset);
        }
end
