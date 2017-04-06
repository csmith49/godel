open Core

(* we need to differentiate constant leaves and variables *)
(* TODO *)
let is_variable (s : string) : bool = true

(* effectively parameterizes kbo *)
(* TODO *)
let weight (s : string) : int = 1
let mu : int = 0
let prec (s : string) (t : string) : bool = false

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
    VarBal.exists (fun k v -> v > 0) vb
let no_positive (vb : balance) : bool =
    VarBal.exists (fun k v -> v < 0) vb

(* we tuple variable counting, weight balances, variable containment, all together *)
let rec modify_balances (vb : balance) (wb : int) (t : program) (y : string) (pos : bool) : balance * int * bool =
    match t with
        | Leaf x ->
            if is_variable x then
                if pos then (inc vb x), wb + mu, x = y
                else (dec vb x), wb - mu, x = y
            else vb, wb + (weight x), false
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
let rec tkbo (vb : balance) (wb : int) (s : program) (t : program) : balance * int * ordering =
    match s, t with
        (* case a *)
        | Leaf x, Leaf y -> begin match (is_variable x), (is_variable y) with
            (* true case a *)
            | true, true ->
                let vbp = dec (inc vb x) y in
                let res = if x = y then EQ else NA in
                    vbp, wb, res
            (* short-circuited case b *)
            | true, false ->
                let vbp, wbp, _ = modify_balances vb wb t x false in
                let vbpp = inc vbp x in
                    vbpp, wbp + mu, NA
            (* and c *)
            | false, true ->
                let vbp, wbp, _ = modify_balances vb wb s y true in
                let vbpp = dec vbp y in
                    vbpp, wbp - mu, NA
            (* and d *)
            | false, false ->
                let vbp, wbp, _ = tkbo_aux vb wb x y [] [] in
                let wbpp = wbp + (weight x) - (weight y) in
                let g_or_n = if no_negative vbp then GT else NA in
                let l_or_n = if no_positive vbp then LT else NA in
                    if wbpp > 0 then vbp, wbpp, g_or_n
                    else if wbpp < 0 then vbp, wbpp, l_or_n
                    else if prec x y then vbp, wbpp, g_or_n
                    else if prec y x then vbp, wbpp, l_or_n
                    else if not (x = y) then vbp, wbpp, NA
                    else vbp, wbpp, EQ
        end
        (* case b *)
        | Leaf x, Node (g, ts) ->
            let vbp, wbp, ctn = modify_balances vb wb t x false in
            let res = if ctn then LT else NA in
            let vbpp = inc vbp x in
                vbpp, wbp + mu, res
        (* case c *)
        | Node (f, ss), Leaf y ->
            let vbp, wbp, ctn = modify_balances vb wb s y true in
            let res = if ctn then GT else NA in
            let vbpp = dec vbp y in
                vbpp, wbp - mu, res
        (* case d *)
        | Node (f, ss), Node (g, ts) ->
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