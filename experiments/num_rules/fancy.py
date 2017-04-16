# sadly, requires python2.7 (or just not python3)
from __future__ import division
import seaborn as sns
from argparse import ArgumentParser
import csv

# eh, arguments and stuff
parser = ArgumentParser("Graph generation for CSV files.")
parser.add_argument("-o", "--output", default="graph.png")
args = parser.parse_args()

# load up the file
def load_data(filename):
    data = []
    with open(filename, "r") as f:
        reader = csv.reader(f, delimiter="\t")
        for row in reader:
            data.append(map(float, row))
    return list(map(list, zip(*data)))

# and print the results
sns.set_style("white")
# load more data
bu = load_data("data/eq_min_pair_bu.csv")
bus = load_data("data/eq_min_pair_bu_old.csv")

sns.tsplot(bu, color="r")
sns.tsplot(bus)

sns.despine()
sns.plt.savefig(args.output)
