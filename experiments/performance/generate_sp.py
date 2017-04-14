import csv
from collections import defaultdict
from glob import glob
from argparse import ArgumentParser
import os
import pandas as pd
import seaborn as sns

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

def get_overhead(bm):
    return bm["NK_n"] / bm["NK_t"]

def get_norm(bm):
    wo = bm["_p"]
    w = bm["NK_p"]
    if wo == 0:
        return 0
    else:
        return (wo - w) / wo

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("data")
    parser.add_argument("--output", "-o", default="scatterplot.png")
    args = parser.parse_args()

    data = load_data(args.data)
    # make clean bu data
    bu = data.copy()
    bu.index = ["{}_bu".format(i) for i in bu.index]
    bu["strategy"] = bu.apply(lambda x: "BU", axis=1)
    bu["_p"] = bu["BU_p"]
    bu["NK_n"] = bu["BUNK_n"]
    bu["NK_t"] = bu["BUNK_t"]
    bu["NK_p"] = bu["BUNK_p"]
    bu["_t"] = bu["BU_t"]
    # and td data
    td = data.copy()
    td.index = ["{}_td".format(i) for i in td.index]
    td["strategy"] = td.apply(lambda x: "TD", axis=1)
    td["_p"] = td["TD_p"]
    td["NK_n"] = td["TDNK_n"]
    td["NK_t"] = td["TDNK_t"]
    td["NK_p"] = td["TDNK_p"]
    td["_t"] = td["TD_t"]
    # join them all together
    data = bu.append(td)[["strategy", "_p", "NK_n", "NK_t", "NK_p", "_t"]]

    data["%N"] = data.apply(get_norm, axis=1)
    data["%O"] = data.apply(get_overhead, axis=1)

    sns.set(style="white")
    f_data = data[(data["%N"] > 0) & (data["_t"] > 1)]

    sns.jointplot(x="%N", y="%O", data=f_data, kind="kde")
    sns.plt.show()
