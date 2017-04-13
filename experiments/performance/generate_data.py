from subprocess import STDOUT, check_output, TimeoutExpired
from argparse import ArgumentParser
import sys
import csv

parser = ArgumentParser("Generation of CSV data for performance benchmarking.")
parser.add_argument("benchmark")
parser.add_argument("--verbose", "-v", default=False, action="store_true")
parser.add_argument("--iterations", "-i", default=10, type=int)
parser.add_argument("--output", "-o", default="data.csv")
parser.add_argument("--timeout", "-t", default=300, type=int)
args = parser.parse_args()

# the rules, which we need because we can't use -dense
RULES = [
    "../../rules/avg.scm",
    "../../rules/base.scm",
    "../../rules/bool.scm",
    "../../rules/int_compare.scm",
    "../../rules/int_extended.scm",
    "../../rules/int.scm",
    "../../rules/list.scm",
    "../../rules/string.scm",
    "../../rules/tuple.scm"
]
RULESTRING = " ".join(map(lambda r: "-rule {}".format(r), RULES))
# the executable
GODEL = "../../godel.native"
# the commands
cmds = [(k, v.format(godel=GODEL, bm=args.benchmark, rules=RULESTRING)) for k, v in [
    ("BU", "{godel} -target {bm} -stats"),
    ("BUN", "{godel} -target {bm} -stats -no-kbo -dtree {rules}"),
    ("BUNK", "{godel} -target {bm} -stats -dtree {rules}"),
    ("TD", "{godel} -target {bm} -stats -strategy td"),
    ("TDN", "{godel} -target {bm} -stats -strategy td -no-kbo -dtree {rules}"),
    ("TDNK", "{godel} -target {bm} -stats -strategy td -dtree {rules}")
]]
# our container for data

# cleanly wrap a command execution with a timeout handler
def run_command(cmd):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=args.timeout, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)

# process godel's output to get the execution time, norm time, and number of programs visited
def get_data(output):
    try:
        lines = output.split("\n")
        # grab the time
        time_line = list(filter(lambda l: l.startswith("TIME:"), lines))[0]
        time = float(time_line.split()[-1])
        # the norm time
        norm_line = list(filter(lambda l: l.startswith("NORM TIME:"), lines))[0]
        norm = float(norm_line.split()[-1])
        # and the programs visited
        prog_line = list(filter(lambda l: l.startswith("NUM_PROGS:"), lines))[0]
        prog = int(prog_line.split()[-1])
        return time, norm, prog
    except Exception as e:
        print(e)
        return args.timeout, args.timeout, 0

# repeatedly run experiment to get a single data frame
def get_frame():
    row = []
    for ex, cmd in cmds:
        if args.verbose: print("CHECKING {}...".format(ex))
        time, norm, prog = get_data(run_command(cmd))
        if args.verbose:
            print("\tTOTAL TIME: {}".format(time))
            print("\tNORMs TIME: {}".format(norm))
            print("\t# OF PROGS: {}".format(prog))
        row.extend([time, norm, prog])
    return row

# now get and save all the data
data = []
for i in range(args.iterations):
    if args.verbose:
        print("<== ITERATION {} ==>".format(i+1))
    data.append(get_frame())

fields = []
for ex, _ in cmds:
    fields.extend(map(lambda f: f.format(ex), ["{}_t", "{}_n", "{}_p"]))

# write the frames out to file
with open(args.output, "w") as f:
    writer = csv.writer(f, delimiter="\t")
    writer.writerow(fields)
    for row in data:
        writer.writerow(row)
