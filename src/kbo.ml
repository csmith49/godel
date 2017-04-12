open Core

(* we need to differentiate constant leaves and variables *)
let is_variable (s : string) : bool =
    try (String.sub s 0 4) = "HOLE"
    with Invalid_argument _ -> false

(* effectively parameterizes kbo *)
(* we hold the strict values in ref maps *)
module ValMap = Map.Make(String)
type vmap = int ValMap.t
let weights : vmap ref = ref ValMap.empty
let precs : vmap ref = ref ValMap.empty
(* and update them as we go *)
let add_weight (s : string) (w : int) : unit =
    weights := ValMap.add s w !weights
let add_prec (s : string) (p : int) : unit =
    precs := ValMap.add s p !precs
(* so that we can pull the information when we need *)
let get_weight (s : string) : int =
    try ValMap.find s !weights
    with Not_found -> 1
let get_prec (s : string) : int =
    try ValMap.find s !precs
    with Not_found -> 0
(* and turn into our easy access *)
let weight (s : string) : int = get_weight s
let mu : int = 0
let prec (s : string) (t : string) : bool =
    let sp = get_prec s in
    let tp = get_prec t in
        sp > tp

(* in order to maintain a linear traversal, must keep track of variable balances *)
module VarBal = Map.Make(String)
type balance = int VarBal.t

let inc (vb : balance) (x : string) : balance =
    let i = try VarBal.find x vb with Not_found -> 0 in
        VarBal.add x (i + 1) vb
let dec (vb : balance) (x : string) : balance =
    let i = try VarBal.find x vb with Not_found -> 0 in
        VarBal.add x (i - 1) vb

let no_negative (vb : balance) : bool =
    not (VarBal.exists (fun k v -> v < 0) vb)
let no_positive (vb : balance) : bool =
    not (VarBal.exists (fun k v -> v > 0) vb)

let string_of_varbal (vb : balance) : string =
    let out = ref "" in
    let f = fun k v -> out := !out ^ ", " ^ k ^ " -> " ^ (string_of_int v) in
    let _ = VarBal.iter f vb in
        "(" ^ !out ^ ")"

(* we tuple variable counting, weight balances, variable containment, all together *)
let rec modify_balances (vb : balance) (wb : int) (t : program) (y : string) (pos : bool) : balance * int * bool =
    match t with
        | Leaf x ->
            if is_variable x then
                if pos then (inc vb x), wb + mu, x = y
                else (dec vb x), wb - mu, x = y
            else
                if pos then vb, wb + (weight x), false
                else vb, wb - (weight x), false
        | Node (f, ss) ->
            let vbp, wbp, res = modify_balances_lex vb wb ss y pos in
                if pos then vbp, wbp + (weight f), res
                else vbp, wbp - (weight f), res
and modify_balances_lex (vb : balance) (wb : int) (ss : program list) (y : string) (pos : bool) : balance * int * bool =
    match ss with
        | [] -> vb, wb, false
        | t :: ts ->
            let vbp, wbp, res = modify_balances vb wb t y pos in
                if not res then modify_balances_lex vbp wbp ts y pos
                else let vbpp, wbpp, _ = modify_balances_lex vbp wbp ts y pos
                    in vbpp, wbpp, true

(* we can find more precise orderings *)
type ordering = GT | LT | EQ | NA

(* which finally gives us this monstrosity of a tupled kbo check *)
let rec v_and_v (vb : balance) (wb: int) (x : string) (y : string) =
    let vbp = dec (inc vb x) y in
    let res = if x = y then EQ else NA in
        vbp, wb, res
and v_and_f (vb : balance) (wb : int) (x : string) (g : string) (ts : program list) =
    let vbp, wbp, ctn = modify_balances vb wb (Node (g, ts)) x false in
    let res = if ctn then LT else NA in
    let vbpp = inc vbp x in
        vbpp, wbp + mu, res
and f_and_v (vb : balance) (wb : int) (f : string) (ss : program list) (y : string) =
    let vbp, wbp, ctn = modify_balances vb wb (Node (f, ss)) y true in
    let res = if ctn then GT else NA in
    let vbpp = dec vbp y in
        vbpp, wbp - mu, res
and f_and_f (vb : balance) (wb : int) (f : string) (ss : program list) (g : string) (ts : program list) =
    let vbp, wbp, lex = tkbo_aux vb wb f g ss ts in
    let wbpp = wbp + (weight f) - (weight g) in
    let g_or_n = if no_negative vbp then GT else NA in
    let l_or_n = if no_positive vbp then LT else NA in
        if wbpp > 0 then vbp, wbpp, g_or_n
        else if wbpp < 0 then vbp, wbpp, l_or_n
        else if prec f g then vbp, wbpp, g_or_n
        else if prec g f then vbp, wbpp, l_or_n
        else if not (f = g) then vbp, wbpp, NA
        else if lex = EQ then vbp, wbpp, EQ
        else if lex = GT then vbp, wbpp, g_or_n
        else if lex = LT then vbp, wbpp, l_or_n
        else vbp, wbpp, NA
and tkbo (vb : balance) (wb : int) (s : program) (t : program) : balance * int * ordering =
    match s, t with
        (* case a *)
        | Leaf x, Leaf y -> begin match (is_variable x), (is_variable y) with
            (* true case a *)
            | true, true -> v_and_v vb wb x y
            (* short-circuited case b *)
            | true, false -> v_and_f vb wb x y []
            (* and c *)
            | false, true -> f_and_v vb wb x [] y
            (* and d *)
            | false, false -> f_and_f vb wb x [] y []
        end
        (* case b *)
        | Leaf x, Node (g, ts) -> v_and_f vb wb x g ts
        (* case c *)
        | Node (f, ss), Leaf y -> f_and_v vb wb f ss y
        (* case d *)
        | Node (f, ss), Node (g, ts) -> f_and_f vb wb f ss g ts
and tkbo_aux (vb : balance) (wb : int) (f : string) (g : string) (ss : program list) (ts : program list) : balance * int * ordering =
    if f = g then tkbo_lex vb wb ss ts
    else
        let vbp, wbp, _ = modify_balances_lex vb wb ss "" true in
        let vbpp, wbpp, _ = modify_balances_lex vbp wbp ts "" false in
            vbpp, wbpp, NA
and tkbo_lex (vb : balance) (wb : int) (ss : program list) (ts : program list) : balance * int * ordering =
    match ss, ts with
        | [], [] -> vb, wb, EQ
        | x :: xs, y :: ys ->
            let vbp, wbp, res = tkbo vb wb x y in
                if res = EQ then tkbo_lex vbp wbp xs ys
                else
                    let vbpp, wbpp, _ = modify_balances_lex vbp wbp xs "" true in
                    let vbppp, wbppp, _ = modify_balances_lex vbpp wbpp ys "" false in
                        vbppp, wbppp, res
        | _, _ -> failwith "list length invariant broken"

(* to finally wrap and get the real kbo *)
let kbo (s : program) (t : program) : ordering =
    let vb = VarBal.empty in
    let _, _, res = tkbo vb 0 s t in
        res

(* or the thing we really care about *)
let gt (s : program) (t : program) : bool =
    (kbo s t) = GT

(* besides maybe loading from file *)
let vlist_of_sexp xs =
    let paired = function
        | Sexplib.Sexp.List ((Sexplib.Sexp.Atom n) :: (Sexplib.Sexp.Atom v) :: []) ->
            (n, (int_of_string v))
        | _ -> failwith "not a valid pair"
    in List.map paired xs

let load_file (filename : string) : unit =
    let parse_command s = match s with
        | Sexplib.Sexp.List ((Sexplib.Sexp.Atom "weights") :: xs) ->
            List.iter (fun (k, v) -> add_weight k v) (vlist_of_sexp xs)
        | Sexplib.Sexp.List ((Sexplib.Sexp.Atom "precs") :: xs) ->
            List.iter (fun (k, v) -> add_prec k v) (vlist_of_sexp xs)
        | _ -> ()
    in begin match (Sexplib.Sexp.load_sexp filename) with
        | Sexplib.Sexp.List xs -> List.iter parse_command xs
        | _ -> failwith "not a valid file"
    end;
