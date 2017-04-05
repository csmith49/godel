open Sexplib
open Core
open Cache

(* UTILITY FUNCTIONS *)

(* taken from ocaml 99 probs *)
let rec random_selection (l : 'a list) (n : int) : ('a list) =
    let rec extract acc n = function
        | [] -> raise Not_found
        | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
    in let extract_random l len =
        extract [] (Random.int len) l
    in let rec aux n acc l len =
        if n = 0 then
            acc
        else
            let picked, rest = extract_random l len in
                aux (n - 1) (picked :: acc) rest (len - 1)
    in let len = List.length l in
        aux (min n len) [] l len

(* monadic treatment of the option type *)
let liftOM f a b = match a, b with
    | Some a, Some b -> f a b
    | _, None -> None
    | None, _ -> None

(* check if a string represents a hole *)
let is_hole (s : string) : bool =
    try (String.sub s 0 4) = "HOLE"
    with Invalid_argument _ -> false

(* and what's a variable for pattern-matching purposes *)
(* is this actually right? *)
let is_var : (program -> bool) = function
    | Leaf c -> is_hole c
    | _ -> false

(* and if a program is linear *)
let rec is_linear : (program -> bool) = function
    | Leaf _ -> true
    | Node (f, args) ->
        if (List.length args) <= 1 then
            List.for_all is_linear args
        else false

(* lexicographic lifting *)
let rec lex_lift (f : 'a -> 'a -> bool) (x : 'a list) (y : 'a list) : bool =
    match x, y with
        | x :: xs, y :: ys ->
            if (f x y) then true
            else if x = y then (lex_lift f xs ys)
            else false
        | _ -> false

(* utility to prevent short-circuits *)
let rec list_or : (bool list -> bool) = function
    | [] -> false
    | x :: xs -> if x then true else (list_or xs)

(* we also need to be recording variable counts *)
module Counter = Map.Make(String)
let rec counter_of_list (l : string list) : int Counter.t =
    match l with
        | x :: xs ->
            let rest = (counter_of_list xs) in
            if (Counter.mem x rest) then
                Counter.add x ((Counter.find x rest) + 1) rest
            else
                Counter.add x 1 rest
        | [] -> Counter.empty
(* which we use in part for kbo stuff *)
let geq_count (c : int Counter.t) (k : string) (v : int) : bool =
    try (Counter.find k c) >= v
    with Not_found -> false
let eq_count (c : int Counter.t) (k : string) (v : int) : bool =
    try (Counter.find k c) = v
    with Not_found -> false
(* once we've counted holes *)
let rec get_holes : (program -> string list) = function
    | Leaf c -> if (is_hole c) then [c] else []
    | Node (f, args) -> List.flatten (List.map get_holes args)
let num_holes (p : program) : int Counter.t =
    counter_of_list (get_holes p)

(* DATA TYPES *)

(* valmaps store string -> int maps *)
module ValMap = Map.Make(String)
let rec valmap_of_list l = match l with
    | (k, v) :: xs -> ValMap.add k v (valmap_of_list xs)
    | [] -> ValMap.empty
let valmap_of_sexp xs =
    let paired = function
        | Sexp.List ((Sexp.Atom n) :: (Sexp.Atom v) :: []) ->
            (n, (int_of_string v))
        | _ -> failwith "not a valid pair"
    in valmap_of_list (List.map paired xs)

(* patterns are the bread and butter of our approach *)
type pattern =
    | PFunc of string * pattern list
    | PConst of string
    | PVar of string

module Pattern = struct
    type t = pattern

    (* we load from sexp files *)
    let rec of_sexp (s : Sexp.t) : t =
        let is_var c = (c.[0] = '@') in
            match s with
                | Sexp.Atom s -> if (is_var s) then (PVar s) else (PConst s)
                | Sexp.List (x :: xs) ->
                    PFunc (Sexp.to_string x, List.map of_sexp xs)
                | Sexp.List [] -> PConst "[]"
    (* and convert to-from strings in the manner of sexps *)
    let rec to_string (p : t) : string = match p with
        | PVar x -> x
        | PConst c -> c
        | PFunc (f, xs) ->
            let sep a b = a ^ " " ^ b in
            let args = List.map to_string xs in
                "(" ^ f ^ (List.fold_left sep "" args) ^ ")"
    let of_string (s : string) : t = of_sexp (Sexp.of_string s)

    (* default structural comparison *)
    let compare = Pervasives.compare

    (* of course, we want patterns to match on terms and make subs *)
    module Substitution = Map.Make(String)
    (* we combine subs in pattern matching *)
    let merge_sub s1 s2 =
        let f sub (k, v) = match sub with
            | Some sub ->
                if (Substitution.mem k sub) && (Substitution.find k sub) <> v then
                    None
                else Some (Substitution.add k v sub)
            | None -> None
        in List.fold_left f (Some s1) (Substitution.bindings s2)
    (* and eventually pattern match *)
    let rec match_with_exp (p : t) (exp : program) : (program Substitution.t) option =
        match p, exp with
            | PVar x, t -> Some (Substitution.singleton x t)
            | PFunc (g, args), Node (f, arfs) ->
                if g = f then
                    let subproblems = List.map2 match_with_exp args arfs in
                    let f = liftOM merge_sub in
                    let acc = Some Substitution.empty in
                        List.fold_left f acc subproblems
                else None
            | PConst s, Leaf c ->
                if s = c then
                    Some Substitution.empty
                else None
            | PFunc (f, []), Leaf c ->
                if f = c then
                    Some Substitution.empty
                else None
            | _ -> None
    (* with sub application *)
    let rec apply_sub (p : t) (sub : program Substitution.t) : program =
        match p with
            | PFunc (f, args) -> Node (f, List.map (fun a -> apply_sub a sub) args)
            | PConst c -> Leaf c
            | PVar x -> Substitution.find x sub
end

(* we order programs by kbo *)
type kbo = {
    weights : int ValMap.t;
    precs : int ValMap.t
}
module KBO = struct
    type t = kbo

    (* we build up a kbo in parts *)
    let empty : t = {
        weights = ValMap.empty;
        precs = ValMap.empty;
    }
    let merge (kbo1 : t) (kbo2 : t) : t =
        let f k l r = Some (max l r) in
            {
                weights = (ValMap.union f kbo1.weights kbo2.weights);
                precs = (ValMap.union f kbo1.precs kbo2.precs);
            }

    (* and need to look up weights and precs often *)
    let symbol_weight (symbol : string) (k : t) : int =
        try (ValMap.find symbol k.weights)
        with Not_found -> 2
    let symbol_prec (symbol : string) (k : t) : int =
        try (ValMap.find symbol k.precs)
        with Not_found -> 0
    (* which we lift to programs *)
    (* this part should probably be hashed *)
    let rec weight (prog : program) (k : t) : int = match prog with
        | Leaf c -> if (is_var prog) then 1 else (symbol_weight c k)
        | Node (f, args) ->
            let r kid = weight kid k in
            let kid_weights = List.map r args in
            (symbol_weight f k) + (List.fold_left (+) 0 kid_weights)
    let prec (prog : program) (k : t) : int = match prog with
        | Leaf c -> symbol_prec c k
        | Node (f, args) -> symbol_prec f k

    (* importantly, we use kbos to compare progs *)
    let rec greater_than (s : program) (t : program) (k : t) : bool =
        (* break out the relevant values *)
        let w_s, w_t = weight s k, weight t k in
        let p_s, p_t = prec s k, prec t k in
        let x_s, x_t = num_holes s, num_holes t in
        (* and then compare *)
        (* case 1 *)
        if (w_s > w_t) && (Counter.for_all (geq_count x_s) x_t) then true
        (* case 2 *)
        else if (w_s = w_t) && (Counter.for_all (eq_count x_s) x_t) then
        (* case 2 a *)
            if (is_var t) && (is_linear s) then true
                (* case 2 b *)
            else if p_s > p_t then true
            (* case 2 c *)
            else match s, t with
                | Node (f, arfs), Node (g, args) -> lex_lift (fun a b -> greater_than a b k) arfs args
                | _ -> false
        else false
end

(* and rules just label pairs of patterns with order or not *)
type rule = Rule of pattern * pattern | Equation of pattern * pattern

module Rule = struct
    type t = rule

    (* default structural comparison *)
    let compare = Pervasives.compare

    (* easily convert from sexps *)
    let rule_of_sexp = function
        | Sexp.List (l :: [r]) -> Rule (Pattern.of_sexp l, Pattern.of_sexp r)
        | _ -> failwith "not a valid rule"
    let equation_of_sexp = function
        | Sexp.List (l :: [r]) -> Equation (Pattern.of_sexp l, Pattern.of_sexp r)
        | _ -> failwith "not a valid equation"

    (* fancy printing *)
    let to_string : (t -> string) = function
        | Rule (lhs, rhs) -> (Pattern.to_string lhs) ^ " -> " ^ (Pattern.to_string rhs)
        | Equation (lhs, rhs) -> (Pattern.to_string lhs) ^ " == " ^ (Pattern.to_string rhs)

    (* several kinds of application *)
    let reduces_wrt_kbo (prog : program) (lhs : Pattern.t) (rhs : Pattern.t) (k : KBO.t) : bool =
        match (Pattern.match_with_exp lhs prog) with
            | Some s ->
                let left, right = (Pattern.apply_sub lhs s), (Pattern.apply_sub rhs s) in
                    KBO.greater_than left right k
            | None -> false
    (* final application check - is the rule worthwhile? *)
    let applicable (prog : program) (r : t) (k : KBO.t) : bool = match r with
        | Rule (lhs, _) ->
            begin match (Pattern.match_with_exp lhs prog) with
                | Some _ -> true
                | None -> false
            end
        | Equation (lhs, rhs) ->
            if (reduces_wrt_kbo prog lhs rhs k) then
                true
            else
                (reduces_wrt_kbo prog rhs lhs k)
end

(* for stat-keeping purposes, we need to map from rules *)
module StatsMap = Map.Make(Rule)

(* where the stats look like these *)
type stat = {
    size : int;
    time : float;
    successes : int;
    failures : int;
}

(* of course, we'll convert default from rules *)
module Stat = struct
    type t = stat
    (* we really only care about the sides *)
    let of_rule_aux lhs rhs =
        {
            size = 1;
            time = 0.0;
            successes = 0;
            failures = 0;
        }
    (* so we'll just pattern match out the sides and apply *)
    let of_rule : (Rule.t -> t) = function
        | Rule (l, r) -> of_rule_aux l r
        | Equation (l, r) -> of_rule_aux l r
    (* easy update for stats after application *)
    let update (s : t) (time_spent : float) (applied : bool) : t =
        match applied with
            | true ->
                {s with
                    time = s.time +. time_spent;
                    successes = s.successes + 1;
                }
            | false ->
                {s with
                    time = s.time +. time_spent;
                    failures = s.failures + 1;
                }

    (* and printing is important to actually do anything with the stats *)
    let to_string (s : t) : string =
        let s1 = string_of_int s.size in
        let s2 = string_of_float s.time in
        let s3 = string_of_int s.successes in
        let s4 = string_of_int s.failures in
            s1 ^ "\t" ^ s2 ^ "\t" ^ s3 ^ "\t" ^ s4
end

let rec statsmap_of_rule_list = function
    | r :: rs ->
        let fresh_stat = Stat.of_rule r in
        let other_stats = statsmap_of_rule_list rs in
        StatsMap.add r fresh_stat other_stats
    | [] -> StatsMap.empty

(* now, a system just wraps it all together *)
type system = {
    comparison : KBO.t;
    rules : rule list;
    mutable stats : stat StatsMap.t;
    cache : Cache.t
}

module System = struct
    type t = system

    (* so we can build things up piece-meal *)
    let empty : t = {
        comparison = KBO.empty;
        rules = [];
        stats = StatsMap.empty;
        cache = Cache.empty;
    }
    let merge (s1 : t) (s2 : t) : t = let f k l r = Some (max l r) in
        {
            comparison = KBO.merge s1.comparison s2.comparison;
            rules = s1.rules @ s2.rules;
            stats = StatsMap.union f s1.stats s2.stats;
            cache = Cache.merge s1.cache s2.cache;
        }

    let random_subset (s : t) (k : int) : t = {
        s with
        rules = random_selection s.rules k
    }

    (* primarily load from files *)
    let of_file (filename : string) : t =
        let w, p, r = ref ValMap.empty, ref ValMap.empty, ref [] in
        let parse_command s = match s with
            | Sexp.List ((Sexp.Atom "rules") :: xs) ->
                r := !r @ (List.map Rule.rule_of_sexp xs)
            | Sexp.List ((Sexp.Atom "eqs") :: xs) ->
                r := !r @ (List.map Rule.equation_of_sexp xs)
            | Sexp.List ((Sexp.Atom "weights") :: xs) ->
                w := valmap_of_sexp xs
            | Sexp.List ((Sexp.Atom "precs") :: xs) ->
                p := valmap_of_sexp xs
            | _ -> failwith "nope"
        in begin match (Sexp.load_sexp filename) with
            | Sexp.List xs -> List.iter parse_command xs
            | _ -> failwith "not a valid file"
        end;
        {
            comparison = {
                weights = !w;
                precs = !p;
            };
            rules = !r;
            stats = statsmap_of_rule_list !r;
            cache = Cache.empty;
        }

    (* print some stuff *)
    let print_stats (s : t) =
        let p = fun k v -> print_endline ((Rule.to_string k) ^ "\t" ^ (Stat.to_string v)) in
        StatsMap.iter p s.stats

    (* finally, normality checks *)
    let rec normal (prog : program) (s : t) : bool =
        if is_var prog then true
        else let f r = Rule.applicable prog r s.comparison in
            if List.exists f s.rules then false
            else match prog with
                | Node (_, args) -> List.for_all (fun a -> normal a s) args
                | _ -> true
    let root_normal (prog : program) (s : t) : bool =
        let f r = Rule.applicable prog r s.comparison in
        not (List.exists f s.rules)

    (* and the annotated versions *)
    let check (prog : program) (r : Rule.t) (s : t) : bool =
        let current_time = Sys.time () in
        let answer = Rule.applicable prog r s.comparison in
        let time_spent = (Sys.time ()) -. current_time in
        let old_stat = StatsMap.find r s.stats in
        let new_stat = Stat.update old_stat time_spent answer in
        begin
            s.stats <- StatsMap.add r new_stat s.stats;
            answer
        end
    (* be careful using these - they don't short-circuit *)
    (* okay, normality checking will short out at that level *)
    let rec a_normal (prog : program) (s : t) : bool =
        if is_var prog then true
        else let f = (fun r -> check prog r s) in
            if (list_or (List.map f s.rules)) then false
            else match prog with
                | Node (_, args) -> List.for_all (fun a -> a_normal a s) args
                | _ -> true
    let a_root_normal (prog : program) (s : t) : bool =
        let f = (fun r -> check prog r s) in
            not (list_or (List.map f s.rules))
end

(* cached normality checks *)
let rec c_weight (p : Program.t) (s : System.t) : int =
    match (Cache.find s.cache.weight p) with
        | Some ans -> ans
        | None -> let report ans = Cache.add s.cache.weight p ans in match p with
            | Leaf c -> if (is_var p) then report 1 else report (KBO.symbol_weight c s.comparison)
            | Node (f, args) ->
                let r = fun k -> c_weight k s in
                let k_weights = List.map r args in
                    report (KBO.symbol_weight f s.comparison) + (List.fold_left (+) 0 k_weights)

let rec c_greater_than (l : Program.t) (r : Program.t) (s : System.t) : bool =
    match (Cache.find s.cache.kbo (l, r)) with
        | Some ans -> ans
        | None ->
            let report ans = Cache.add s.cache.kbo (l, r) ans in
            let w_l, w_r = c_weight l s, c_weight r s in
            let p_l, p_r = KBO.prec l s.comparison, KBO.prec r s.comparison in
            let x_l, x_r = num_holes l, num_holes r in
            (* case 1 *)
            if (w_l > w_r) && (Counter.for_all (geq_count x_l) x_r) then report true
            (* case 2 *)
            else if (w_l = w_r) && (Counter.for_all (eq_count x_l) x_r) then
                (* case 2 a *)
                if (is_var r) && (is_linear l) then report true
                (* case 2 b *)
                else if p_l > p_r then report true
                else match l, r with
                    | Node (f, arfs), Node (g, args) -> report (lex_lift (fun a b -> c_greater_than a b s) arfs args)
                    | _ -> report false
            else report false

let c_reduces_wrt_kbo (p : Program.t) (lhs : Pattern.t) (rhs : Pattern.t) (s : System.t) : bool =
    match (Pattern.match_with_exp lhs p) with
        | Some sub -> let left, right = (Pattern.apply_sub lhs sub), (Pattern.apply_sub rhs sub) in
            c_greater_than left right s
        | None -> false

let c_applicable (p : Program.t) (r : Rule.t) (s : System.t) : bool =
    match r with
        | Rule (lhs, _) ->
            begin match (Pattern.match_with_exp lhs p) with
                | Some _ -> true
                | None -> false
            end
        | Equation (lhs, rhs) ->
            if (c_reduces_wrt_kbo p lhs rhs s) then true
            else (c_reduces_wrt_kbo p rhs lhs s)

let rec c_normal (prog : Program.t) (s : System.t) : bool =
    match (Cache.find s.cache.normal prog) with
        | Some ans -> ans
        | None -> if is_var prog then true
            else let report ans = Cache.add s.cache.normal prog ans in
                let f r = c_applicable prog r s in
                    if List.exists f s.rules then report false
                    else match prog with
                        | Node (_, args) ->
                            report (List.for_all (fun a -> c_normal a s) args)
                        | _ -> report true

let c_root_normal (prog : Program.t) (s : System.t) : bool =
    let f = fun r -> c_applicable prog r s in
        not (List.exists f s.rules)
