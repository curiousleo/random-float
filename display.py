import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":
    from sys import stdin
    xs = np.loadtxt(stdin, dtype=np.float64)

    unique, counts = np.unique(xs, return_counts=True)
    diff = np.ediff1d(unique, to_begin=0, to_end=0) / 2
    area = (diff[1:] + diff[:-1])

    plt.rcParams.update({'font.size': 22})
    fig, axs = plt.subplots(2, 1, sharex=True)

    axs[0].stem(unique, counts, use_line_collection=True)
    axs[0].set_ylabel("Occurrences")

    axs[1].stem(unique, area, use_line_collection=True)
    axs[1].set_ylabel("Interval size")

    plt.show()
