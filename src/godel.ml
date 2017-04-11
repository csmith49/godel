open Core
open Components
open Tasks
open Config
open Eval
open Search
open Kbo

let _ =
    let start_time = Sys.time() in
    (* parse the command line arguments *)

    Search.synthesize !Config.target;

    if !Config.stats then begin
        print_endline "\n==> STATS ==>";
        Printf.printf "TIME: %f\n" (Sys.time() -. start_time);
        Printf.printf "NORM TIME: %f\n" (!Config.normalize_time);
        Printf.printf "NUM_PROGS: %i\n" !program_count;
        (* System.print_stats !Config.system; *)
    end
