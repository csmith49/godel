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
    data = pd.DataFrame.from_csv(f)
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

def get_stats(data):
    timing_data = data.groupby("size")["time"]

    means = timing_data.mean()
    stdev = timing_data.std()

    return means, stdev

# get a run
def sample_from_stats(means, stdev):
    output = []
    for m, std in zip(means, stdev):
        # try:
        #     value = np.random.normal(m, std)
        # except:
        #     value = m
        value = m
        output.append(max(value, 0))
    return output

# construct a time-series
def make_frame(means, stdev, runs=100):
    output = []
    for i in range(runs):
        output.append(sample_from_stats(means, stdev))
    return output

# defining how we extract the relevant data and make tha graphs
# deprecated
def generate_graph(data, experiment):
    # unpack the relevant experimental stuff
    strat, kbo = experiment
    # what are we gonna write this as
    file_name = "{}_{}.png".format(strat, "kbo" if kbo else "no-kbo")
    # now pull out the important stuff
    data = data[data["strat"] == strat]
    data = data[data["kbo"] == kbo]
    # split it up into normal-form and not
    norm_data = data[data["norm"] == True].groupby("size")["time"].mean()
    not_data = data[data["norm"] == False].groupby("size")["time"].mean()
    # now we can graph
    plt.figure()
    # set some style stuff
    rc={'axes.labelsize': 28, 'legend.fontsize': 24, 'axes.titlesize': 36, 'xtick.labelsize': 20, 'ytick.labelsize': 20}
    sns.set(style="white", rc=rc)
    # plot the data
    plt.plot(norm_data, label="Normal")
    plt.plot(not_data, label="Removed")
    plt.legend()
    plt.xlabel("Program size")
    plt.ylabel("Time (in seconds)")
    # and save it to file
    plt.savefig(os.path.join(args.output, file_name))

for exp in EXPERIMENTS:
    # filter the data and split
    exp_data = filter_data(*exp, data)
    norm, pruned = split_data(exp_data)

    # make a simple graph

    # style stuff
    rc={'axes.labelsize': 28, 'legend.fontsize': 24, 'axes.titlesize': 36, 'xtick.labelsize': 20, 'ytick.labelsize': 20}
    sns.set(style="white", rc=rc)

    # clear the plots
    sns.plt.figure()

    # extract and create confidence plots
    norm_ax = sns.tsplot(make_frame(*get_stats(norm), 100), color="r")
    pruned_ax = sns.tsplot(make_frame(*get_stats(pruned), 100), ax=norm_ax)

    # format the axes
    norm_ax.set_xlabel("Program size")
    norm_ax.set_ylabel("Time (microseconds)")

    # set up a legend
    sns.plt.legend([norm_ax, pruned_ax], labels=["normal", "removed"])

    # minor formatting
    sns.plt.tight_layout()
    sns.despine()

    # and save
    file_name = "{}_{}.png".format(exp[0], "kbo" if exp[1] else "no-kbo")
    sns.plt.savefig(os.path.join(args.output, file_name))
