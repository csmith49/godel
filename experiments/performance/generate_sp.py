import csv
from collections import defaultdict
from glob import glob
from argparse import ArgumentParser
import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def load_file(filename):
    output = defaultdict(lambda: 0)
    row_count = 0
    with open(filename, 'r') as f:
        reader = csv.DictReader(f, delimiter="\t")
        for row in reader:
            row_count += 1
            for experiment, value in row.items():
                output[experiment] += float(value)
    return {ex : (v / row_count) for ex, v in output.items()}

def load_data(dirname):
    data_files = glob(os.path.join(dirname, "*"))
    output = {}
    for f in data_files:
        name = os.path.splitext(os.path.basename(f))[0]
        data = load_file(f)
        output[name] = data
    return pd.DataFrame(output).T

def get_frame(data, bu, kbo):
    output = pd.DataFrame()
    # choose the relevant kbo string
    if kbo: kbo_string = "K"
    else: kbo_string = ""
    # and the relevant strat string
    if bu: strat_string = "BU"
    else: strat_string = "TD"
    # filter by the non-terminating results
    base_time = "{}_t".format(strat_string)
    data = data[(data[base_time] < 300) & (data[base_time] > 1)]
    # now let's get our overhead
    n_n = "{}N{}_n".format(strat_string, kbo_string)
    n_t = "{}N{}_t".format(strat_string, kbo_string)
    output["overhead"] = data.apply(lambda e: e[n_n] / e[n_t], axis=1)
    # our normalization percent
    p = "{}_p".format(strat_string)
    n_p = "{}N{}_p".format(strat_string, kbo_string)
    output["reduction"] = data.apply(lambda e: (e[p] - e[n_p]) / e[p], axis=1)
    # and now add our labels
    output["strategy"] = output.apply(lambda e: strat_string, axis=1)
    output["normalization"] = output.apply(lambda e: "ordered" if kbo else "unordered", axis=1)
    return output

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("data")
    parser.add_argument("--output", "-o", default="scatterplot.png")
    parser.add_argument("--topdown", "-t", default=False, action="store_true")
    parser.add_argument("--kbo", "-k", default=False, action="store_true")
    args = parser.parse_args()

    # load in the data, split it up into howver many frames
    raw_data = load_data(args.data)
    data = get_frame(raw_data, args.topdown, args.kbo)
    print(data)

    # now we worry about graphing
    rc={'font.size': 32, 'axes.labelsize': 24, 'legend.fontsize': 32.0, 'axes.titlesize': 32, 'xtick.labelsize': 16, 'ytick.labelsize': 16}
    sns.set(style="white", rc=rc)
    # sns.set_color_codes("pastel")
    g = sns.JointGrid(x="reduction", y="overhead", data=data, xlim=(0,1), ylim=(0,1), space=0)
    g = g.plot_joint(sns.kdeplot, shade=True, shade_lowest=False,cmap="BuGn")
    g = g.plot_marginals(sns.kdeplot, shade=True, color="g")
    g.ax_joint.set_xticks([0,0.2, 0.4, 0.6, 0.8, 1])
    g.ax_joint.set_yticks([0,0.2, 0.4, 0.6, 0.8, 1])

    sns.plt.savefig(args.output)
