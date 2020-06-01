import numpy as np
import matplotlib.pyplot as plt

def explode(xs):
    xsabs = np.abs(xs)
    b_s = 52 if xs.dtype == np.float64 else 23
    b_e = 11 if xs.dtype == np.float64 else 8
    bias = 1023 if xs.dtype == np.float64 else 127
    dtype = np.uint64 if xs.dtype == np.float64 else np.uint32

    ns = xs < 0
    es = (xsabs.view(dtype) >> b_s) & ((1<<b_e)-1)
    ss = xsabs.view(dtype) & ((1<<b_s)-1)

    assert(ns.size == xs.size)
    assert(es.size == xs.size)
    assert(ss.size == xs.size)

    return (ns, es, ss)

if __name__ == "__main__":
    from sys import stdin
    xs = np.loadtxt(stdin, dtype=np.float32)
    #(ns, es, ss) = explode(xs)
    #print(xs)
    #print(ns)
    #print(es)
    #print(ss)

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
