# sadly, requires python2.7 (or just not python3)
from __future__ import division
import seaborn as sns
import numpy as np
from argparse import ArgumentParser
import csv

# eh, arguments and stuff
parser = ArgumentParser("Graph generation for CSV files.")
parser.add_argument("-o", "--output", default="")
args = parser.parse_args()

# load up the file
def load_data(filename):
    data = []
    with open(filename, "r") as f:
        reader = csv.reader(f, delimiter="\t")
        for row in reader:
            data.append(map(float, row))
    return list(map(list, zip(*data)))

# load more data
bu = load_data("data/eq_str_len_bu.csv")
bus = load_data("data/eq_str_len_bu_old.csv")

# set up some formatting
rc={'axes.labelsize': 24, 'legend.fontsize': 16, 'axes.titlesize': 32, 'xtick.labelsize': 16, 'ytick.labelsize': 16}
sns.set(style="white", rc=rc)

# and plot the data
bu_ax = sns.tsplot(bu, color="r" )
bus_ax = sns.tsplot(bus, ax=bu_ax)
# now set the ticks
n = len(bu_ax.xaxis.get_ticklabels())
bu_ax.set_xticklabels(list(map(int, np.linspace(0, 150, n))))
# and the labels
bu_ax.set_xlabel("Number of rules")
bu_ax.set_ylabel("Time (seconds)")
# set upt he legend
sns.plt.legend([bu_ax, bus_ax], labels=["d-tree", "list"])


sns.despine()
if args.output != "":
    sns.plt.savefig(args.output)
else:
    sns.plt.show()
