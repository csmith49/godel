open Core
open Components
open Patterns
open Tasks
open Config
open Eval
open Search

let _ =
    let start_time = Sys.time() in
    (* parse the command line arguments *)

    Search.synthesize !Config.target;

    if !Config.stats then begin
        print_endline "\n==> STATS ==>";
        Printf.printf "TIME: %f\n" (Sys.time() -. start_time);
        Printf.printf "NUM_PROGS: %i\n" !program_count;
        System.print_stats !Config.system;
    end
