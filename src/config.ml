open Patterns

(* the flags we care about and will reference elsewhere *)
let target = ref "";;
let max_height = ref 100;;
let expansion_metric = ref "size";;
let expansion_strategy = ref "bu";;
let symmetry_reduction = ref false;;
let system = ref System.empty;;
let reduce = ref false;;
let kbo = ref true;;
let noisy = ref false;;
let enumerate = ref false;;
let program_count = ref 0;;
(* for recent stats *)
let stats = ref false;;
let rule_stats = ref false;;
let subset = ref 0;;

let process_rules filename = begin
    system := System.merge (System.of_file filename) !system;
    reduce := true;
end;;

let dense_rules unused = begin
    let files = [
        "./rules/base.scm";
        "./rules/int.scm";
        "./rules/bool.scm";
        "./rules/int_compare.scm";
        "./rules/int_extended.scm";
        "./rules/tuple.scm";
        "./rules/string.scm";
        "./rules/list.scm"
    ] in
    let f s = system := System.merge s !system in
    List.iter f (List.map System.of_file files);
    reduce := true;
end;;

(* arguments for converting cmd line args into vars above *)
let arg_list = [
    ("-target", Arg.Set_string target, " Target program.");
    ("-metric", Arg.Set_string expansion_metric, " Set expansion metric.");
    ("-strategy", Arg.Set_string expansion_strategy, " Set expansion strategy.");
    ("-noisy", Arg.Set noisy, " Enable noisy output.");
    ("-enumerate", Arg.Set enumerate, " Disable termination on success.");
    ("-rule", Arg.String process_rules, " Add a set of rules for EQ-RED.");
    ("-dense", Arg.Unit dense_rules, " Enable ALL rules for EQ-RED.");
    ("-no-kbo", Arg.Clear kbo, " Disable ordered rules.");
    ("-stats", Arg.Set stats, " Enables detailed output stats.");
    ("-rule-stats", Arg.Set rule_stats, " Enables detailed rule stats.");
    ("-subset", Arg.Set_int subset, " Chooses a random subset of rules.")
];;

let anon_fun = (fun s -> raise (Arg.Bad (s ^ " is not a recognized argument"))) in
let usage_msg = "Let's synthesize some stuff!" in
    Arg.parse (Arg.align arg_list) anon_fun usage_msg;
    Random.self_init ();
    if !subset != 0 then
        system := System.random_subset !system !subset;
