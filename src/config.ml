(* the flags we care about and will reference elsewhere *)
let target = ref "";;
let max_height = ref 100;;
let expansion_metric = ref "size";;
let expansion_strategy = ref "bu";;
let symmetry_reduction = ref false;;
let reduce = ref false;;
let kbo = ref true;;
let noisy = ref false;;
let enumerate = ref false;;
let program_count = ref 0;;
(* for recent stats *)
let use_dtree = ref false;;
let stats = ref false;;
let subset = ref 0;;
let dtree = ref Normal.DTree.empty;;
let system = ref Normal.System.empty;;
let normalize_time = ref 0.0;;
let visualize_dtree = ref false;;
let popl = ref false;;

let to_process : (string list ref) = ref [];;
let register (f : string) : unit =
    to_process := f :: !to_process;;

let process_rules filename = begin
    print_string ("loading file: " ^ filename ^ "...");
    (* register kbo data *)
    Kbo.load_file filename;
    (* and to the system *)
    system := Normal.System.merge (Normal.System.of_file filename !kbo) !system;
    (* and set it so we actually reduce *)
    reduce := true;
    print_endline "done.";
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
    List.iter register files;
end;;

(* arguments for converting cmd line args into vars above *)
let arg_list = [
    ("-target", Arg.Set_string target, " Target program.");
    ("-metric", Arg.Set_string expansion_metric, " Set expansion metric.");
    ("-strategy", Arg.Set_string expansion_strategy, " Set expansion strategy.");
    ("-noisy", Arg.Set noisy, " Enable noisy output.");
    ("-enumerate", Arg.Set enumerate, " Disable termination on success.");
    ("-rule", Arg.String register, " Add a set of rules for EQ-RED.");
    ("-dense", Arg.Unit dense_rules, " Enable ALL rules for EQ-RED.");
    ("-no-kbo", Arg.Clear kbo, " Disable ordered rules.");
    ("-stats", Arg.Set stats, " Enables detailed output stats.");
    ("-subset", Arg.Set_int subset, " Chooses a random subset of rules.");
    ("-dtree", Arg.Set use_dtree, " Enables Discrimination Tree normalization.");
    ("-visualize", Arg.Set visualize_dtree, " Prints the generated Discrimination Tree.");
    ("-popl", Arg.Set popl, " Enables checking for POPL experiments");
];;

let anon_fun = (fun s -> raise (Arg.Bad (s ^ " is not a recognized argument"))) in
let usage_msg = "Let's synthesize some stuff!" in
    (* initialize our randomizer *)
    Random.self_init ();
    (* update the commands *)
    Arg.parse (Arg.align arg_list) anon_fun usage_msg;
    (* if there are rules to process, do so *)
    if (List.length !to_process) != 0 then begin
        List.iter process_rules !to_process;
        reduce := true;
    end;
    (* if we need to get rid of random rules and whatnot, do so *)
    if !subset != 0 then
        system := Normal.System.random_subset !system !subset;
    (* if we're using the dtree, make it from the system now *)
    if !use_dtree then
        dtree := Normal.System.to_dtree !system;
    (* aaaaaand maybe print the tree *)
    if !visualize_dtree then
        print_endline (Normal.DTree.to_string !dtree);
