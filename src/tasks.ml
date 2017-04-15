open Core
open Components

type task =
    {
        target : component;
        inputs : Vector.t list;
        components : component list;
        rec_components : component list
    }

let int_list xs = VList (List.map (fun x -> VInt x) xs)
let make_pair x y = VList [VInt x; VInt y]

let full_components =
    IntComponents.components @ IntCompareComponents.components @ BoolComponents.components

(* EQ-RED TASK HELPERS *)
let int_inputs = [|
        (Leaf "a", [| VInt 2; VInt 6; VInt 100 |]);
        (Leaf "b", [| VInt 2; VInt 7; VInt 8 |]);
        (Leaf "c", [| VInt 4; VInt 18; VInt 7 |]);
        (Leaf "d", [| VInt 3; VInt 11; VInt 5 |]);
        (Leaf "e", [| VInt 1; VInt 11; VInt 0 |])|]

let pair_inputs = [
    (Leaf "a", [|
            make_pair 1 4;
            make_pair 780 342;
            make_pair 123 512|]);
    (Leaf "b", [|
            make_pair 12 86;
            make_pair 0 5;
            make_pair 157 832|])]

let str_math_inputs = [
    (Leaf "a", [|
            VString "20";
            VString "16";
            VString "100"
        |]);
    (Leaf "b", [|
            VString "19";
            VString "49";
            VString "8"
    |])
]

let int_task ?(extended=false) ?(arity=2) t = {
    target = t;
    inputs = Array.to_list (Array.sub int_inputs 0 arity);
    rec_components = [];
    components = let base = IntComponents.components in
        if extended then
            base @ IntCompareComponents.components @ BoolComponents.components
        else base
}

let pair_task ?(extended=false) t = {
    target = t;
    inputs = pair_inputs;
    rec_components = [];
    components =
        let base = IntComponents.components @ TupleComponents.components in
            if extended then base @ IntExtendedComponents.components
            else base
}

(* simple ~binary tasks *)
let eq_add = int_task eq_add
let eq_max = int_task ~extended:true eq_max
let eq_min = int_task ~extended:true eq_min
let eq_add_3 = int_task ~extended:true ~arity:3 eq_add_3
let eq_add_4 = int_task ~arity:4 eq_add_4
let eq_add_5 = int_task ~arity:5 eq_add_5

let int_tasks = [eq_add;eq_max;eq_min;eq_add_3;eq_add_4;eq_add_5]

(* rational tasks *)
let eq_add_q = pair_task eq_add_q
let eq_sub_q = pair_task eq_sub_q
let eq_mult_q = pair_task eq_mult_q
let eq_div_q = pair_task eq_div_q

let rational_tasks = [eq_add_q;eq_sub_q;eq_mult_q;eq_div_q]

(* irrational *)
let eq_add_c = pair_task eq_add_c
let eq_sub_c = pair_task eq_sub_c
let eq_mult_c = pair_task eq_mult_c

let irrational_tasks = [eq_add_c;eq_sub_c;eq_mult_c]

(* min/max domains *)
let eq_max_pair = pair_task ~extended:true eq_max_pair
let eq_intervals = pair_task ~extended:true eq_intervals
let eq_min_pair = pair_task ~extended:true eq_min_pair

let minmax_tasks = [eq_max_pair;eq_intervals;eq_min_pair]

(* pair tasks *)
let eq_sum_to_first = {(pair_task eq_sum_to_first) with components = (
        IntComponents.min_components @ TupleComponents.components
    )}
let eq_sum_to_second = {(pair_task eq_sum_to_second) with components = (
        IntComponents.min_components @ TupleComponents.components
    )}
let eq_sum_to_first_and_second = pair_task eq_sum_to_first_and_second
let eq_max_and_acc = pair_task ~extended:true eq_max_and_acc
let eq_add_and_mult = pair_task eq_add_and_mult
let eq_distances = pair_task eq_distances

let pair_tasks = [eq_sum_to_first;eq_sum_to_first_and_second;eq_max_and_acc;
    eq_add_and_mult; eq_distances; eq_sum_to_second]

(* misc *)
let eq_inner_product = pair_task eq_inner_product

(* long rational tasks *)
let rational_task t = {
    target = t;
    inputs = Array.to_list (Array.sub int_inputs 0 4);
    rec_components = [];
    components = IntComponents.min_components @ TupleComponents.components
}

let eq_add_q_long = rational_task eq_add_q_long
let eq_sub_q_long = rational_task eq_sub_q_long
let eq_mult_q_long = rational_task eq_mult_q_long
let eq_div_q_long = rational_task eq_div_q_long

let long_rat_tasks = [eq_add_q_long; eq_sub_q_long;
    eq_mult_q_long; eq_div_q_long]

(* string math tasks *)
let string_math_task t = {
    target = t;
    inputs = str_math_inputs;
    rec_components = [];
    components = IntComponents.components @ StringComponents.components @ IntCompareComponents.components @ BoolComponents.components
}

let eq_str_add = string_math_task eq_str_add
let eq_str_mult = string_math_task eq_str_mult
let eq_str_max = string_math_task eq_str_max

let string_math_tasks = [eq_str_add; eq_str_max; eq_str_mult]

(* more stringy things *)
let eq_str_split = {
    target = eq_str_split;
    inputs = [
        (Leaf "a", [|
            VString "A a a";
            VString "Very words";
            VString "these benches hit the mark"
        |]);
        (Leaf "b", [|
            VString "ottf";
            VString "a bit a";
            VString "pneumonoultramicroscopisilicovolcaniconiosis"
        |])
    ];
    rec_components = [];
    components = StringComponents.min_components @ IntCompareComponents.min_components @ ListComponents.min_components
}

let eq_str_len = {
    target = eq_str_len;
    inputs = [
        (Leaf "a", [|
            VString "A a a";
            VString "Very words         ";
            VString "these benches hit the mark"
        |]);
        (Leaf "b", [|
            VString " ottf";
            VString "   a bit a";
            VString "pneumonoultramicroscopisilicovolcaniconiosis"
        |])
    ];
    rec_components = [];
    components = StringComponents.components @ IntCompareComponents.components @ IntComponents.components
}

let eq_str_trim_len = {
    target = eq_str_trim_len;
    inputs = [
        (Leaf "a", [|
            VString "A a a";
            VString "Very words         ";
            VString "these benches hit the mark"
        |]);
        (Leaf "b", [|
            VString " ottf     ";
            VString "   a bit a";
            VString "pneumonoultramicroscopisilicovolcaniconiosis"
        |])
    ];
    rec_components = [];
    components = StringComponents.components @ IntCompareComponents.min_components
}

let eq_str_upper_len = {
    target = eq_str_upper_len;
    inputs = [
        (Leaf "a", [|
            VString "A a a";
            VString "Very words         ";
            VString "these benches hit the mark"
        |]);
        (Leaf "b", [|
            VString " ottf     ";
            VString "   a bit a";
            VString "pneumonoultramicroscopisilicovolcaniconiosis"
        |])
    ];
    rec_components = [];
    components = StringComponents.components @ IntCompareComponents.min_components
}

let eq_str_lower_len = {
    target = eq_str_lower_len;
    inputs = [
        (Leaf "a", [|
            VString "A a a";
            VString "Very words         ";
            VString "these benches hit the mark"
        |]);
        (Leaf "b", [|
            VString " ottf     ";
            VString "   a bit a";
            VString "pneumonoultramicroscopisilicovolcaniconiosis"
        |])
    ];
    rec_components = [];
    components = StringComponents.components @ IntCompareComponents.min_components
}

let string_tasks = [eq_str_split; eq_str_len; eq_str_trim_len; eq_str_upper_len; eq_str_lower_len]

(* list tasks *)
let list_inputs = [
    (Leaf "a", [|
        int_list [1; 2; 3; 4; 5];
        int_list [3; 2; 1];
        int_list [0]|]);
    (Leaf "b", [|
        int_list [11; 0; 4];
        int_list [1; 1];
        int_list [-1; 3; 6]|])]

let list_task t = {
    target = t;
    inputs = list_inputs;
    rec_components = [];
    components = IntComponents.components @ IntCompareComponents.components @
        ListComponents.components @ BoolComponents.components @ IntExtendedComponents.components
}

let eq_ls_sum = list_task eq_ls_sum
let eq_ls_sum2 = list_task eq_ls_sum2
let eq_ls_sum_abs = list_task eq_ls_sum_abs
let eq_ls_min = list_task eq_ls_min
let eq_ls_max = list_task eq_ls_max
let eq_ls_stutter = {(list_task eq_ls_stutter) with components = (
        IntComponents.min_components @ ListComponents.min_components @
        IntExtendedComponents.components)}

let list_tasks = [eq_ls_sum; eq_ls_min; eq_ls_max; eq_ls_stutter; eq_ls_sum2; eq_ls_sum_abs]

(* random tasks *)
let eq_avg_pair = {
    target = eq_avg_pair;
    inputs = [
        (Leaf "a", [|
            VList [int_list [1;2;3]; VInt 4];
            VList [int_list [0;0;2;4;5]; VInt 0];
            VList [int_list [100;99;1]; VInt 1]
        |]);
        (Leaf "b", [|
            VList [int_list [5;7;11;92]; VInt 6];
            VList [int_list [2]; VInt (-1)];
            VList [int_list [82; 3]; VInt 3]
        |])];
    rec_components = [];
    components = IntComponents.bot_components @ TupleComponents.bot_components @ ListComponents.bot_components
}

let random_tasks = [eq_avg_pair]

(* pull it all together *)
let synthesis_targets = int_tasks @ rational_tasks @
    irrational_tasks @ minmax_tasks @ pair_tasks @
    long_rat_tasks @ string_math_tasks @ string_tasks @
    list_tasks @ random_tasks
