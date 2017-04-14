from subprocess import STDOUT, check_output, TimeoutExpired
from argparse import ArgumentParser
import sys
import csv

parser = ArgumentParser("Generation of CSV data for timing experiments.")
parser.add_argument("benchmark")
parser.add_argument("--verbose", "-v", default=False, action="store_true")
parser.add_argument("--iterations", "-i", default=10, type=int)
parser.add_argument("--rules", "-r", nargs="+")
parser.add_argument("--maxsize", "-m", default=10, type=int)
parser.add_argument("--output", "-o", default="data.csv")
parser.add_argument("--topdown", "-t", default=False, action="store_true")
args = parser.parse_args()

rule_string = " ".join(list(map(lambda s: "-rule " + s, args.rules)))
CMD = "../../godel.native -target {} -subset {} -stats {} -dtree".format(args.benchmark, "{}", rule_string)
if args.topdown:
    CMD = CMD + " -strategy td"

# cleanly wrap a command execution with a timeout handler
def run_command(cmd, seconds):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=seconds, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)

# process godel's output to get the execution time
def get_time(output, default):
    try:
        lines = output.split("\n")
        time_line = list(filter(lambda l: l.startswith("TIME:"), lines))[0]
        return float(time_line.split()[-1])
    except Exception as e:
        print(e)
        return default

# repeatedly run experiment to get a single data frame
def get_frame(size):
    timeout = 300
    output = []
    if args.verbose:
        print("Computing frame: {}".format(size))
    for i in range(args.iterations):
        raw_output = run_command(CMD.format(size), timeout)
        time_output = get_time(raw_output, timeout)
        if args.verbose:
            print("--> {}".format(time_output))
        output.append(time_output)
    return output

# now get and save all the data
data = []
for size in range(args.maxsize):
    data.append(get_frame(size))

# write the frames out to file
with open(args.output, "w") as f:
    writer = csv.writer(f, delimiter="\t")
    for row in data:
        writer.writerow(row)
