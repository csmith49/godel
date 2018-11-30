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
parser.add_argument("--strategy", "-s", default=False, action="store_true")
args = parser.parse_args()

rule_string = " ".join(list(map(lambda s: "-rule " + s, args.rules)))
CMD = "../../godel.native -no-kbo -target {} -subset {} -stats {}".format(args.benchmark, "{}", rule_string)
if not args.strategy:
    CMD = CMD + " -dtree"
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
    if args.verbose:
        print("Computing frame: {}".format(size))
    for i in range(args.iterations):
        raw_output = run_command(CMD.format(size), timeout)
        time_output = get_time(raw_output, timeout)
        if args.verbose:
            print("--> {}".format(time_output))
        yield time_output

# now get and save all the data
def data():
    for size in range(1, args.maxsize, 5):
        for time in get_frame(size):
            yield size, time

# write the frames out to file
with open(args.output, "w", newline='') as f:
    fields = ['rules', 'time']
    writer = csv.DictWriter(f, fieldnames=fields)
    writer.writeheader()
    for size, time in data():
        writer.writerow({"rules": size, "time": time})
