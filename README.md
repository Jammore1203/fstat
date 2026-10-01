# fstat

![CI](https://github.com/Jammore1203/fstat/actions/workflows/ci.yml/badge.svg)

Quick descriptive statistics for a column of numbers, from the shell. Written in modern Fortran (2018) with no dependencies.

Point it at a CSV or pipe numbers in, and it prints the count, sum, min/max, mean, standard deviation, percentiles and a histogram. Headers and non-numeric lines are skipped and counted.

```console
$ fstat -c 3 -b 5 tests/latency.csv
count     12   (skipped 1)
sum       729.600
min       11.9000
max       230.700
mean      60.8000
stddev    63.4648
p25       13.8250
median    47.1000
p75       90.1000
p90       102.040
p99       216.631

       11.90 .. 55.66        |######################################## 8
       55.66 .. 99.42        |##########                               2
       99.42 .. 143.2        |#####                                    1
       143.2 .. 186.9        |                                         0
       186.9 .. 230.7        |#####                                    1
```

```console
$ du -k * | fstat -q                  # how big are the files here?
$ awk '{print $NF}' access.log | fstat   # response-time distribution
```

## Usage

```
fstat [-c COL] [-d DELIM] [-b BINS] [-q] [FILE]

  -c COL    column to summarise, 1-based (default 1)
  -d DELIM  field delimiter (default: comma, tab or runs of spaces)
  -b BINS   histogram bins (default 10)
  -q        no histogram
```

## Build

Needs `gfortran` (GCC 9+).

```bash
make            # builds build/fstat
make test       # unit tests + CLI golden-output test
make install    # copies to /usr/local/bin (PREFIX=... to change)
```

## Implementation notes

- **Variance** uses Welford's online algorithm, so it stays accurate when values sit on a large offset (e.g. Unix timestamps), where the naive sum-of-squares formula loses precision. There's a test for this.
- **Percentiles** interpolate linearly between closest ranks, the same as NumPy's default and Python's `statistics.quantiles(method="inclusive")`.
- **Sorting** is a bottom-up merge sort: O(n log n) worst case and no recursion.
- Input lines can be any length, and the value array grows geometrically with `move_alloc`.
