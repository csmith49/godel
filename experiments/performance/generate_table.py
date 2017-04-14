import csv
from collections import defaultdict
from glob import glob
from argparse import ArgumentParser
import os

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
    return output

def write_data(data, fields, filename):
    with open(filename, "w") as f:
        writer = csv.writer(f, delimiter="\t")
        writer.writerow([""] + fields)
        for ex, d in data.items():
            writer.writerow([ex] + [d[k] for k in fields])

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("data")
    parser.add_argument("--output", "-o", default="table.csv")
    parser.add_argument("--tags", "-t", nargs="+", default=["t"], choices=["t", "n", "p"])
    args = parser.parse_args()

    BMs = ["BU", "BUN", "BUNK", "TD", "TDN", "TDNK"]
    FIELDS = [f for sublist in [list(map(lambda k: "{}_{}".format(ex, k), args.tags)) for ex in BMs] for f in sublist]

    data = load_data(args.data)
    write_data(data, FIELDS, args.output)
