import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import argparse
import os

# constants, man. what're you gonna do
EXPERIMENTS = [
    ("bu", True),
    ("bu", False),
    ("td", True),
    ("td", False)
]

# set up the cmdline args
parser = argparse.ArgumentParser()
parser.add_argument('-o', '--output', default='graphs')
parser.add_argument('-d', '--data')
args = parser.parse_args()

# load up the data
print("Loading data...")
with open(args.data, "r") as f:
    data = pd.read_csv(f)
print("...done.")

# we need to scale data tho
data["time"] = data["time"].apply(lambda t: 1000000 * t)

# pull stats
def filter_data(strat, kbo, data):
    data = data[data["strat"] == strat]
    return data[data["kbo"] == kbo]

def split_data(data):
    norm_data = data[data["norm"] == True]
    not_data = data[data["norm"] == False]
    return norm_data, not_data

for exp in EXPERIMENTS:
    # filter the data and split
    exp_data = filter_data(*exp, data)

    norm = exp_data[exp_data["norm"] == True]
    pruned = exp_data[exp_data["norm"] == False]

    # make a simple graph

    # style stuff
    # rc={'axes.labelsize': 28, 'legend.fontsize': 24, 'axes.titlesize': 36, 'xtick.labelsize': 20, 'ytick.labelsize': 20}
    sns.set(style="white")
    sns.set_style("ticks")
    sns.set_context("paper", font_scale=2.7)

    # clear the plots
    plt.figure()

    # extract and create confidence plots
    # norm_ax = sns.lineplot(make_frame(*get_stats(norm), 100), color="r")
    # pruned_ax = sns.lineplot(make_frame(*get_stats(pruned), 100), ax=norm_ax)
    norm_ax = sns.lineplot(data=norm, x="size", y="time", color="r", ci=None, label="normal")
    pruned_ax = sns.lineplot(data=pruned, x="size", y="time", color="b", ax=norm_ax, ci=None, label="removed")

    # format the axes
    norm_ax.set_xlabel("Program size")
    norm_ax.set_ylabel("Time (in μs)")

    import matplotlib.lines as mlines
    norm_handle = mlines.Line2D([], [], color="r", label="normal")
    pruned_handle = mlines.Line2D([], [], color="b", label="removed")

    # set up a legend
    plt.legend(handles=[norm_handle, pruned_handle])

    # minor formatting
    plt.tight_layout()
    sns.despine()

    # and save
    file_name = "{}_{}.pdf".format(exp[0], "kbo" if exp[1] else "no-kbo")
    plt.savefig(os.path.join(args.output, file_name))
