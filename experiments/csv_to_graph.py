# sadly, requires python2.7 (or just not python3)
from __future__ import division
import seaborn as sns
from argparse import ArgumentParser
import csv

# eh, arguments and stuff
parser = ArgumentParser("Graph generation for CSV files.")
parser.add_argument("input")
parser.add_argument("-t", "--title")
parser.add_argument("-x", "--xlabel")
parser.add_argument("-y", "--ylabel")
parser.add_argument("-o", "--output", default="graph.png")
parser.add_argument("--transpose", default=False, action="store_true")
args = parser.parse_args()

# load up the file
data = []
with open(args.input, "r") as f:
    reader = csv.reader(f, delimiter="\t")
    for row in reader:
        data.append(map(float, row))

# and just in case we want to interchange rows and columns...
if args.transpose:
    data = list(map(list, zip(*data)))

# and print the results
sns.set_style("white")
sns.tsplot(data, err_style="unit_traces")
if args.xlabel:
    sns.plt.xlabel(args.xlabel)
if args.ylabel:
    sns.plt.ylabel(args.ylabel)
if args.title:
    sns.plt.title(args.title)
sns.despine()
sns.plt.savefig(args.output)
