import subprocess
from argparse import ArgumentParser
import pandas as pd
import sys

# get the input arguments
parser = ArgumentParser()
parser.add_argument("-b", "--benchmark")
parser.add_argument("-n", "--noisy", default=False, action="store_true")
parser.add_argument("-w", "--width", default=50, type=int)
parser.add_argument("-o", "--output", default="data.csv")
parser.add_argument("-t", "--timeout", default=600, type=int)
args = parser.parse_args()

# our basic command
CMD = "cd ../..; ./godel.native -target {bm} -dense -dtree -popl -width {width} -strategy {strat} {kbo} -enumerate"

# cleanly wrap a command execution with a timeout handler
def run_command(cmd):
    output = subprocess.run(cmd, stdout=subprocess.PIPE, shell=True)
    return output.stdout.decode(sys.stdout.encoding)

# the experiments are pretty straightforward:
# filter out the POPL output, convert to csv
#   we will repeat for each strategy and kbo/no-kbo
def run_experiment(benchmark, strategy, kbo):
    cmd = CMD.format(
        bm=benchmark,
        strat=strategy,
        kbo= "" if kbo else "-no-kbo",
        width=args.width
    )
    output = []
    raw_data = run_command(cmd)
    for line in raw_data.split("\n"):
        try:
            _, size, norm, time = line.split(",")
            output.append({
                "strat" : strategy,
                "kbo" : kbo,
                "size" : int(size),
                "norm" : norm == "true",
                "time" : float(time)
            })
        except:
            pass
    return output

# let's do some actual experimentation
configs = [("bu", True), ("bu", False), ("td", True), ("td", False)]
# some data config stuff
data = []
columns = ["strat", "kbo", "size", "norm", "time"]
# now loop
for strat, kbo in configs:
    exp_data = run_experiment(args.benchmark, strat, kbo)
    data = data + exp_data

# and we'll convert the data and write it out
df = pd.DataFrame(data, columns=columns)
with open(args.output, "w") as f:
    df.to_csv(f)
