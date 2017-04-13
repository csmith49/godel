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

# the rules
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
BU = "{godel} -target {bm} -stats".format(godel=GODEL, bm=args.benchmark)
BUN = "{godel} -target {bm} -stats -no-kbo -dtree {rules}".format(godel=GODEL, bm=args.benchmark, rules=RULESTRING)
BUNK = "{godel} -target {bm} -stats -dtree {rules}".format(godel=GODEL, bm=args.benchmark, rules=RULESTRING)
TD = "{godel} -target {bm} -stats -strategy td".format(godel=GODEL, bm=args.benchmark)
TDN = "{godel} -target {bm} -stats -strategy td -no-kbo -dtree {rules}".format(godel=GODEL, bm=args.benchmark, rules=RULESTRING)
TDNK = "{godel} -target {bm} -stats -strategy td -dtree {rules}".format(godel=GODEL, bm=args.benchmark, rules=RULESTRING)

# cleanly wrap a command execution with a timeout handler
def run_command(cmd):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=args.timeout, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)

# process godel's output to get the execution time
def get_time(output):
    try:
        lines = output.split("\n")
        time_line = list(filter(lambda l: l.startswith("TIME:"), lines))[0]
        return float(time_line.split()[-1])
    except Exception as e:
        print(e)
        return args.timeout

# repeatedly run experiment to get a single data frame
def get_frame():
    experiments = [BU, BUN, BUNK, TD, TDN, TDNK]
    output = []
    for e in experiments:
        d = get_time(run_command(e))
        if args.verbose:
            print("--> {}".format(d))
        output.append(d)
    return output

# now get and save all the data
data = [["BU", "BUN", "BUNK", "TD", "TDN", "TDNK"]]
for i in range(args.iterations):
    if args.verbose:
        print("Getting frame {}...".format(i+1))
    data.append(get_frame())

# write the frames out to file
with open(args.output, "w") as f:
    writer = csv.writer(f, delimiter="\t")
    for row in data:
        writer.writerow(row)
