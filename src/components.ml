open Core

type component =
    { domain : typ list;
      codomain : typ;
      apply : value list -> value;
      name : string }

(* applying components to vectors *)
let select i list = List.map (fun x -> x.(i)) list

let apply_component (c : component) (args : Vector.t list) =
  let progs = List.map fst args in
  let values = List.map snd args in
  let result =
    Array.mapi (fun i _ -> c.apply (select i values)) (List.hd values)
  in
    ( (Node (c.name, progs)), result)

(* conditionals to use when we dont' have a goal graph *)
let cond_list =
  { domain = [TBool; TList; TList];
    codomain = TList;
    apply = (function [VBool x; bthen; belse] -> if x then bthen else belse
	       | _ -> VError);
    name = "cond" }

let cond_int =
  { domain = [TBool; TInt; TInt];
    codomain = TInt;
    apply = (function [VBool x; bthen; belse] -> if x then bthen else belse
	       | _ -> VError);
    name = "cond" }

let cond_bool =
  { domain = [TBool; TBool; TBool];
    codomain = TBool;
    apply = (function [VBool x; bthen; belse] -> if x then bthen else belse
	       | _ -> VError);
    name = "cond" }

let cond_tree =
  { domain = [TBool; TTree; TTree];
    codomain = TTree;
    apply = (function [VBool x; bthen; belse] -> if x then bthen else belse
	       | _ -> VError);
    name = "cond" }

let cond_string =
  { domain = [TBool; TString; TString];
    codomain = TString;
    apply = (function [VBool x; bthen; belse] -> if x then bthen else belse
	       | _ -> VError);
    name = "cond" }

(* integers *)
module IntComponents = struct
    let add =
    {
        domain = [TInt; TInt];
        codomain = TInt;
        apply = (function
                    | [VInt a; VInt b] -> VInt (a + b)
                    | _ -> VError);
        name = "add"
    }

    let mult =
    {
        domain = [TInt; TInt];
        codomain = TInt;
        apply = (function
                    | [VInt a; VInt b] -> VInt (a * b)
                    | _ -> VError);
        name = "mult"
    }

    let sub =
    {
        domain = [TInt; TInt];
        codomain = TInt;
        apply = (function
                    | [VInt a; VInt b] -> VInt (a - b)
                    | _ -> VError);
        name = "sub"
    }

    let abs =
    {
        domain = [TInt];
        codomain = TInt;
        apply = (function
                    | [VInt a] -> VInt (abs a)
                    | _ -> VError);
        name = "abs"
    }

    let succ =
    {
        domain = [TInt];
        codomain = TInt;
        apply = (function
                    | [VInt a] -> VInt (a + 1)
                    | _ -> VError);
        name = "succ"
    }

    let zero =
    {
        domain = [];
        codomain = TInt;
        apply = (function
                    | [] -> VInt 0
                    | _ -> VError);
        name = "0"
    }

    let one =
    {
        domain = [];
        codomain = TInt;
        apply = (function
                    | [] -> VInt 1
                    | _ -> VError);
        name = "1"
    }

    let components = [add; mult; sub; abs; zero]
    let min_components = [add; mult; abs; zero]
    let bot_components = [add; zero]
end

(* booleans *)
module BoolComponents = struct
    let eq_and =
    {
        domain = [TBool; TBool];
        codomain = TBool;
        apply = (function
                    | [VBool l; VBool r] -> VBool (l && r)
                    | _ -> VError);
        name = "and"
    }

    let eq_or =
    {
        domain = [TBool; TBool];
        codomain = TBool;
        apply = (function
                    | [VBool l; VBool r] -> VBool (l || r)
                    | _ -> VError);
        name = "or"
    }

    let eq_not =
    {
        domain = [TBool];
        codomain = TBool;
        apply = (function
                    | [VBool b] -> VBool (not b)
                    | _ -> VError);
        name = "not"
    }

    let eq_true =
    {
        domain = [];
        codomain = TBool;
        apply = (fun _ -> VBool true);
        name = "true"
    }

    let eq_false =
    {
        domain = [];
        codomain = TBool;
        apply = (fun _ -> VBool false);
        name = "false"
    }

    let components = [eq_and; eq_or; eq_not; eq_true; eq_false]
end

(* integer comparisons *)
module IntCompareComponents = struct
    let eq_gt =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x > y)
                    | _ -> VError);
        name = "gt"
    }

    let eq_lt =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x < y)
                    | _ -> VError);
        name = "lt"
    }

    let eq_geq =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x >= y)
                    | _ -> VError);
        name = "geq"
    }

    let eq_leq =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x <= y)
                    | _ -> VError);
        name = "leq"
    }

    let eq_eq =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x = y)
                    | _ -> VError);
        name = "eq"
    }

    let eq_neq =
    {
        domain = [TInt; TInt];
        codomain = TBool;
        apply = (function
                    | [VInt x; VInt y] -> VBool (x <> y)
                    | _ -> VError);
        name = "neq"
    }

    let components = [eq_gt; eq_lt; eq_geq; eq_leq; eq_eq; eq_neq]
    let min_components = [eq_gt; eq_lt; eq_eq]
end

(* strings and stuff *)
module StringComponents = struct
    let str_concat = {
        domain = [TString; TString];
        codomain = TString;
        apply = (function
                    | [VString x; VString y] -> VString (x ^ y)
                    | _ -> VError);
        name = "concat"
    }
    let str_length = {
        domain = [TString];
        codomain = TInt;
        apply = (function
                    | [VString x] -> VInt (String.length x)
                    | _ -> VError);
        name = "length"
    }
    let str_upper = {
        domain = [TString];
        codomain = TString;
        apply = (function
                    | [VString s] -> VString (String.uppercase_ascii s)
                    | _ -> VError);
        name = "to_upper"
    }

    let str_to_int = {
        domain = [TString];
        codomain = TInt;
        apply = (function
                    | [VString s] -> begin
                        try VInt (int_of_string s)
                        with _ -> VError end
                    | _ -> VError);
        name = "to_int"
    }

    let str_from_int = {
        domain = [TInt];
        codomain = TString;
        apply = (function
                    | [VInt s] -> VString (string_of_int s)
                    | _ -> VError);
        name = "of_int"
    }

    let str_lower = {
        domain = [TString];
        codomain = TString;
        apply = (function
                    | [VString s] -> VString (String.lowercase_ascii s)
                    | _ -> VError);
        name = "to_lower"
    }
    let rec join li sep = match li with
        | [] -> VString ""
        | ((VString s)::ss) -> begin match (join ss sep) with
            | VString rest -> VString (s ^ sep ^ rest)
            | _ -> VError end
        | _ -> VError
    let str_join = {
        domain = [TList; TString];
        codomain = TString;
        apply = (function
            | [VList ss; VString sep] -> join ss sep
            | _ -> VError);
        name = "join"
    }

    let index str sub =
        let rec go n =
            if n > (String.length str) - (String.length sub) then (-1)
            else if String.sub str n (String.length sub) = sub then n
            else go (n + 1)
        in go 0

    let split str delim = let dl = String.length delim in
        let rec go str = let first = index str delim in
            if first >= 0 then
                (String.sub str 0 first)::(go (String.sub str (first + dl) ((String.length str) - first - dl)))
            else [str]
        in go str

    let str_split = {
        domain = [TString; TString];
        codomain = TList;
        apply = (function
            | [_; VString ""] -> VError
            | [VString w; VString s] ->
                VList (List.map (fun x -> VString x) (split w s))
            | _ -> VError);
        name = "split"
    }

    let str_trim = {
        domain = [TString];
        codomain = TString;
        apply = (function
            | [VString s] -> VString (String.trim s)
            | _ -> VError);
        name = "trim"
    }

    let reverse str =
        let len = String.length str in
        let res = String.create len in
        for i = 0 to pred len do
            let j = pred len - i in
            res.[i] <- str.[j]
        done; res

    let str_rev = {
        domain = [TString];
        codomain = TString;
        apply = (function
            | [VString s] -> VString (reverse s)
            | _ -> VError);
        name = "reverse"
    }
    let components = [
        str_concat;
        str_length;
        str_upper;
        str_lower;
        str_join;
        str_split;
        str_rev;
        str_trim;
        str_to_int;
        str_from_int
    ]
    let min_components = [str_concat; str_length; str_rev; str_trim; str_split]
end

(* tuple constructors and destructors *)
module TupleComponents = struct
    let pair =
    {
        domain = [TInt; TInt];
        codomain = TList;
        apply = (function
            | [VInt x; VInt y] -> VList [VInt x; VInt y]
            | _ -> VError);
        name = "pair"
    }
    let triple =
    {
        domain = [TInt; TInt; TInt];
        codomain = TList;
        apply = (function
            | [VInt x; VInt y; VInt z] -> VList [VInt x; VInt y; VInt z]
            | _ -> VError);
        name = "triple"
    }
    let fst =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
            | [VList xs] when ((List.length xs) >= 1) -> List.hd xs
            | _ -> VError);
        name = "fst"
    }
    let snd =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
            | [VList xs] when ((List.length xs) >= 2) -> List.hd (List.tl xs)
            | _ -> VError);
        name = "snd"
    }
    let thrd =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
            | [VList xs] when ((List.length xs) >= 3) ->
                List.hd (List.tl (List.tl xs))
            | _ -> VError);
        name = "thrd"
    }

    let l_fst = {
        domain = [TList];
        codomain = TList;
        apply = (function
            | [VList xs] when ((List.length xs) >= 1) -> List.hd xs
            | _ -> VError);
        name = "l_fst"
    }
    let components = [pair; fst; snd]
    let bot_components = [pair; l_fst; snd]
end

(* misc. integer functions (min and max) *)
module IntExtendedComponents = struct
    let max =
    {
        domain = [TInt; TInt];
        codomain = TInt;
        apply = (function
            | [VInt x; VInt y] -> VInt (max x y)
            | _ -> VError);
        name = "max"
    }
    let min =
    {
        domain = [TInt; TInt];
        codomain = TInt;
        apply = (function
            | [VInt x; VInt y] -> VInt (min x y)
            | _ -> VError);
        name = "min"
    }
    let components = [max;min]
end

module ListComponents = struct
    let rec sum_impl = function
        | [] -> VInt 0
        | (VInt x::xs) ->
            begin match sum_impl xs with
                | VInt sum -> VInt (x + sum)
                | _ -> VError
            end
        | _ -> VError

    let max_impl x =
        let rec max_impl' x m =
            match x with
            | (VInt x::xs) -> if x > m then max_impl' xs x else
                                max_impl' xs m

            | [] -> VInt m
            | _ -> VError
        in
        max_impl' x 0

    let maxl =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
                    | [VList x] -> max_impl x
                    | _ -> VError);
        name = "maxl"
    }

    let min_impl x =
        let rec min_impl' x m =
            match x with
            | (VInt x::xs) -> if x > m then min_impl' xs x else
                                min_impl' xs m

            | [] -> VInt m
            | _ -> VError
        in
        min_impl' x 0

    let minl =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
                    | [VList x] -> min_impl x
                    | _ -> VError);
        name = "minl"
    }

    let sum =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
                    | [VList x] -> sum_impl x
                    | _ -> VError);
        name = "sum"
    }
    let cons =
    {
        domain = [TInt; TList];
        codomain = TList;
        apply = (function
        | [VInt x; VList xs] -> VList (VInt x::xs)
        | _ -> VError);
        name = "cons"
    }

    let mkList =
    {
        domain = [TInt];
        codomain = TList;
        apply = (function
        | [VInt x] -> VList [VInt x]
        | _ -> VError);
        name = "mkList"
    }

    let cat =
    {
        domain = [TList; TList];
        codomain = TList;
        apply = (function [VList xs; VList ys] -> VList (xs @ ys)
           | _ -> VError);
        name = "cat"
    }

    let length =
    {
        domain = [TList];
        codomain = TInt;
        apply = (function
            | [VList xs] -> VInt (List.length xs)
            | _ -> VError);
        name = "length"
    }

    let rec stutter_impl v i = match i with
        | 0 -> []
        | _ -> v :: (stutter_impl v (i - 1))

    let stutter =
    {
        domain = [TInt; TInt];
        codomain = TList;
        apply = (function
            | [VInt x; VInt i] -> begin
                try VList (List.map (fun v -> VInt v) (stutter_impl x i))
                with _ -> VError
                end
            | _ -> VError);
        name = "stutter"
    }

    let components = [sum;cons;cat;minl;maxl;mkList;length;stutter]
    let min_components = [sum; cat; length; cons; stutter]
    let bot_components = [sum; cat]
end

(* components for testing eq-reduction *)

(* simple binary integer tasks *)
let eq_add = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
                | [VInt a; VInt b] -> VInt (a + b)
                | _ -> VError);
    name = "eq_add"
}

let eq_min = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b] -> VInt (min a b)
        | _ -> VError);
    name = "eq_min"
}

let eq_max = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b] -> VInt (max a b)
        | _ -> VError);
    name = "eq_max"
}

let eq_min = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b] -> VInt (min a b)
        | _ -> VError);
    name = "eq_min"
}

let eq_add_3 = {
    domain = [TInt; TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b; VInt c] -> VInt (a + b + c)
        | _ -> VError);
    name = "eq_add_3"
}

let eq_add_4 = {
    domain = [TInt; TInt; TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d] -> VInt (a + b + c + d)
        | _ -> VError);
    name = "eq_add_4"
}

let eq_add_5 = {
    domain = [TInt; TInt; TInt; TInt; TInt];
    codomain = TInt;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d; VInt e] -> VInt (a + b + c + d + e)
        | _ -> VError);
    name = "eq_add_5"
}

(* rational tasks *)
let eq_add_q = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
            let mult = IntComponents.mult.apply in
                pair [add [mult [fst xs; snd ys]; mult [fst ys; snd xs]];mult [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_add_q"
}

let eq_sub_q = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let sub = IntComponents.sub.apply in
            let mult = IntComponents.mult.apply in
                pair [sub [mult [fst xs; snd ys]; mult [fst ys; snd xs]];mult [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_sub_q"
}

let eq_mult_q = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let mult = IntComponents.mult.apply in
                pair [mult [fst xs; fst ys]; mult [snd ys; snd xs]]
        | _ -> VError);
    name = "eq_mult_q"
}

let eq_div_q = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let mult = IntComponents.mult.apply in
            pair [mult [fst xs; snd ys]; mult [fst ys; snd xs]]
        | _ -> VError);
    name = "eq_div_q"
}

(* irrational arithmetic *)
let eq_add_c = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
                pair [add [fst xs; fst ys]; add [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_add_c"
}

let eq_sub_c = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let sub = IntComponents.sub.apply in
                pair [sub [fst xs; fst ys]; sub [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_sub_c"
}

let eq_mult_c = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let mult = IntComponents.mult.apply in
            let add = IntComponents.add.apply in
            let sub = IntComponents.sub.apply in
            let a, b = fst xs, snd xs in
            let c, d = fst ys, snd ys in
                pair [sub [mult [a;c]; mult [b;d]];add [mult [a;d]; mult [b;c]]]
        | _ -> VError);
    name = "eq_mult_c"
}

(* min/max domains *)
let eq_max_pair = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let max = IntExtendedComponents.max.apply in
                pair [max [fst xs; fst ys]; max [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_max_pair"
}

let eq_intervals = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let max = IntExtendedComponents.max.apply in
            let min = IntExtendedComponents.min.apply in
                pair [min [fst xs; fst ys]; max [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_intervals"
}

let eq_min_pair = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let min = IntExtendedComponents.min.apply in
                pair [min [fst xs; fst ys]; min [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_min_pair"
}

(* misc *)
let eq_constprop = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
            | [VInt a; VInt b] -> if (a = b) then (VInt a) else (VInt (-1))
            | _ -> VError);
    name = "eq_constprop"
}

let eq_longest_string = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString l; VString r] -> if (String.length l) > (String.length r) then
            VString l else VString r
        | _ -> VError);
    name = "eq_longest_string"
}

(* pairs to ints *)
let eq_inner_product = {
    domain = [TList; TList];
    codomain = TInt;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let add = IntComponents.add.apply in
            let mult = IntComponents.mult.apply in
                add [mult [fst xs; fst ys]; mult [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_inner_product"
}

(* pairs of pairs to pairs *)
let eq_sum_to_first = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
                pair [add [add [fst xs; snd xs]; add [fst ys; snd ys]] ; VInt 0]
        | _ -> VError);
    name = "eq_sum_to_first"
}

let eq_sum_to_first_and_second = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
            let v = add [add [fst xs; snd xs]; add [fst ys; snd ys]] in
                pair [v; v]
        | _ -> VError);
    name = "eq_sum_to_first_and_second"
}

let eq_max_and_acc = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
            let max = IntExtendedComponents.max.apply in
            let min = IntExtendedComponents.min.apply in
                pair [max [fst xs; fst ys]; add [min [fst xs; fst ys]; add[snd xs; snd ys]]]
        | _ -> VError);
    name = "eq_max_and_acc"
}

let eq_add_and_mult = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
            let mult = IntComponents.mult.apply in
                pair [add [fst xs; fst ys]; mult [snd xs; snd ys]]
        | _ -> VError);
    name = "eq_add_and_mult"
}

let eq_distances = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let sub = IntComponents.sub.apply in
            let abs = IntComponents.abs.apply in
                pair [abs [sub [fst xs; fst ys]]; abs [sub [snd xs; snd ys]]]
        | _ -> VError);
    name = "eq_distances"
}

(* long versions of rational arithmetic *)
let eq_add_q_long = {
    domain = [TInt; TInt; TInt; TInt];
    codomain = TList;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d] ->
            VList [VInt (a * d + b * c) ; VInt (b * d)]
        | _ -> VError);
    name = "eq_add_q_long"
}

let eq_mult_q_long = {
    domain = [TInt; TInt; TInt; TInt];
    codomain = TList;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d] ->
            VList [VInt (a * b) ; VInt (c * d)]
        | _ -> VError);
    name = "eq_mult_q_long"
}

let eq_sub_q_long = {
    domain = [TInt; TInt; TInt; TInt];
    codomain = TList;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d] ->
            VList [VInt (a * d - b * c) ; VInt (b * d)]
        | _ -> VError);
    name = "eq_sub_q_long"
}

let eq_div_q_long = {
    domain = [TInt; TInt; TInt; TInt];
    codomain = TList;
    apply = (function
        | [VInt a; VInt b; VInt c; VInt d] ->
            VList [VInt (a * d) ; VInt (b * c)]
        | _ -> VError);
    name = "eq_div_q_long"
}

let eq_sum_to_second = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let fst t = List.hd t in
            let snd t = List.hd (List.tl t) in
            let pair = TupleComponents.pair.apply in
            let add = IntComponents.add.apply in
                pair [VInt 0; add [add [fst xs; snd xs]; add [fst ys; snd ys]]]
        | _ -> VError);
    name = "eq_sum_to_second"
}

(* string arithmetic benchmarks *)

let eq_str_add = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] ->
            let to_int = StringComponents.str_to_int.apply in
            let of_int = StringComponents.str_from_int.apply in
            let add = IntComponents.add.apply in
                of_int [add [to_int [VString x]; to_int [VString y]]]
        | _ -> VError);
    name = "eq_str_add"
}

let eq_str_mult = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] ->
            let to_int = StringComponents.str_to_int.apply in
            let of_int = StringComponents.str_from_int.apply in
            let mult = IntComponents.mult.apply in
                of_int [mult [to_int [VString x]; to_int [VString y]]]
        | _ -> VError);
    name = "eq_str_mult"
}

let eq_str_max = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] ->
            let to_int = StringComponents.str_to_int.apply in
            let of_int = StringComponents.str_from_int.apply in
            let max = IntExtendedComponents.max.apply in
                of_int [max [to_int [VString x]; to_int [VString y]]]
        | _ -> VError);
    name = "eq_str_max"
}

(* word count *)
let eq_str_split = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] ->
            let len (VList xs) = List.length xs in
            let split s = StringComponents.str_split.apply [VString s; VString " "] in
            if (len (split x)) > (len (split y)) then
                VString x
            else VString y
        | _ -> VError);
    name = "eq_str_split"
}

let eq_str_len = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] -> let v (VInt i) = i in
            let len = StringComponents.str_length.apply in
                if (v (len [VString x])) > (v (len [VString y])) then
                    VString x
                else VString y
        | _ -> VError);
    name = "eq_str_len"
}

let eq_str_trim_len = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] -> let v (VInt i) = i in
            let len = StringComponents.str_length.apply in
            let binop s = StringComponents.str_trim.apply [VString s] in
                if (v (len [binop x])) > (v (len [binop y])) then
                    binop x
                else binop y
        | _ -> VError);
    name = "eq_str_trim_len"
}

let eq_str_upper_len = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] -> let v (VInt i) = i in
            let len = StringComponents.str_length.apply in
            let binop s = StringComponents.str_upper.apply [VString s] in
                if (v (len [binop x])) > (v (len [binop y])) then
                    binop x
                else binop y
        | _ -> VError);
    name = "eq_str_upper_len"
}

let eq_str_lower_len = {
    domain = [TString; TString];
    codomain = TString;
    apply = (function
        | [VString x; VString y] -> let v (VInt i) = i in
            let len = StringComponents.str_length.apply in
            let binop s = StringComponents.str_lower.apply [VString s] in
                if (v (len [binop x])) > (v (len [binop y])) then
                    binop x
                else binop y
        | _ -> VError);
    name = "eq_str_lower_len"
}


(* a list of list benchmarks *)

let eq_ls_sum = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let ints = List.map (fun (VInt i) -> i) (xs @ ys) in
            VList [VInt (List.fold_left (+) 0 ints)]
        | _ -> VError);
    name = "eq_ls_sum"
}

let eq_ls_sum2 = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let ints = List.map (fun (VInt i) -> i) (xs @ ys) in
            VList [VInt (List.fold_left (+) 2 ints)]
        | _ -> VError);
    name = "eq_ls_sum2"
}

let eq_ls_sum_abs = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let ints = List.map (fun (VInt i) -> i) (xs @ ys) in
            VList [VInt (abs (List.fold_left (+) 0 ints))]
        | _ -> VError);
    name = "eq_ls_sum_abs"
}

let eq_ls_min = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let min = ListComponents.minl.apply in
            VList [min [VList (xs @ ys)]]
        | _ -> VError);
    name = "eq_ls_min"
}

let eq_ls_max = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let max = ListComponents.maxl.apply in
            VList [max [VList (xs @ ys)]]
        | _ -> VError);
    name = "eq_ls_max"
}

let eq_ls_stutter = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
        | [VList xs; VList ys] ->
            let len = ListComponents.length.apply in
            let len_x = len [VList xs] in
            let len_y = len [VList ys] in
            let stutter = ListComponents.stutter.apply in
            let gt = IntCompareComponents.eq_gt.apply in
            begin match gt [len_x; len_y] with
                | VBool true -> stutter [len_x; len_x]
                | VBool false -> stutter [len_y; len_y]
            end
        | _ -> VError);
    name = "eq_ls_stutter"
}

let eq_avg_pair = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function
            | [VList xs; VList ys] ->
                let fst l = List.hd l in
                let snd l = List.hd (List.tl l) in
                let sum = ListComponents.sum.apply in
                let add = IntComponents.add.apply in
                let pair = TupleComponents.pair.apply in
                let cat = ListComponents.cat.apply in
                    pair [ sum [cat [fst xs; fst ys]] ; add [snd xs; snd ys]]
            | _ -> VError);
    name = "eq_avg_pair"
}
