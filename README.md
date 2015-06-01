# Nom benchmarks

This repository holds three different versions of a partial MP4 file parser, written in:

- Rust, with [Geal/nom](https://github.com/Geal/nom)
- Haskell with [bos/attoparsec](https://github.com/bos/attoparsec) and [GaloisInc/cereal](https://github.com/GaloisInc/cereal)
- C with [UpstandingHackers/hammer](https://github.com/UpstandingHackers/hammer)

The goal is to compare their usability and their performance on a real world binary file format. As with all benchmarks, the results must be taken with a grain of salt. This is not a formal comparison of languages, but an experiment to check where the *nom* parser library stands against more established parser libraries, in terms of performance and usability. I welcome any idea or contribution to improve performance for either of the parsers, or improve statistical significance.

The parsers have been written in the most naive way, to make them as deterministic as possible. In each benchmark, the files are completely loaded in memory before measuring, and the parser is applied repeatedly to the buffer in memory. The hammer parser has some slight memory leaks, the developers have been notified of this and the bugs will be fixed in the future.

## Building the benchmarks

# nom

Get the Rust binaries from the [Rust website](http://www.rust-lang.org/install.html).

Go to the top level directory of the repository and run the following command:

```shell
$ cargo bench
     Running target/release/mp4-853bb91a76728946

running 2 tests
test bigbuckbunny_test ... bench:       201 ns/iter (+/- 100)
test small_test        ... bench:       238 ns/iter (+/- 64)

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured
```

To run it without optimizations:

```shell
$ cargo test
[...]
$ ./target/mp4-<hash> --bench

running 2 tests
test bigbuckbunny_test ... bench:      9083 ns/iter (+/- 2193)
test small_test        ... bench:      9619 ns/iter (+/- 1538)

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured
```

# attoparsec and cereal

Get the Haskell binaries from the [Haskell website](https://www.haskell.org/downloads).

Go to the attoparsec subdirectory of the repository, and run the following commands to install all the dependencies:

```shell
$ cabal install --only-dependencies
```

This may take a while. Once everything is downloaded and compiled, run the benchmark like this:

```shell
$ cabal run
Preprocessing executable 'mp4' for mp4-0.1.0.0...
Running mp4...
benchmarking IO/small
time                 1.748 μs   (1.717 μs .. 1.785 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.754 μs   (1.721 μs .. 1.796 μs)
std dev              118.8 ns   (93.31 ns .. 152.0 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking IO/big buck bunny
time                 1.684 μs   (1.646 μs .. 1.731 μs)
                     0.993 R²   (0.989 R² .. 0.997 R²)
mean                 1.714 μs   (1.661 μs .. 1.772 μs)
std dev              180.5 ns   (146.3 ns .. 219.7 ns)
variance introduced by outliers: 89% (severely inflated)
```

# hammer

Clone the [hammer repository](https://github.com/UpstandingHackers/hammer) and follow the instructions in the README to build it.

Then go into the hammer subdirectory and compile the benchmark:

```shell
gcc -o mp4 main.c -I/usr/local/include -L/usr/local/lib -lhammer
```

(this assumes than hammer has been installed in /usr/local).

```shell
$ ./mp4

bench small:
34039.697647 ns/iter (variance: 0.000000)

bench bigbuckbunny:
27943.570614 ns/iter (variance: 0.000004)
```

## Some benchmark results

The results of the commands in this readme come from a late 2013 Macbook Pro, quad core 2,3 GHz Intel Core i7. They have been run with the following parameters:

- Rust: `rustc 1.0.0-dev (522d09dfe 2015-02-19) (built 2015-02-20)`, nom 0.1.6
- Haskell: `Glasgow Haskell Compiler, Version 7.8.4, stage 2 booted by GHC version 7.8.3`, attoparsec-0.12.1.3, attoparsec-binary-0.2
- C: `Apple LLVM version 6.0 (clang-600.0.56) (based on LLVM 3.5svn)`, hammer at commit: UpstandingHackers/hammer#4e8d319d0e33456e6141f24a44dcf94dd5ce346a

### Without optimizations, and with debug symbols

|            | small.mp4 (375 kB)       | bigbuckbunny.mp4 (5.3 MB) |
| ---------- | ------------------------ | ------------------------- |
| hammer     | 32807 ns/iter            | 28115 ns/iter             |
| attoparsec | 1244 ns/iter (+/- 71.7)  | 1183 ns/iter (+/- 60.1)   |
| cereal     | 178 ns/iter (+/- 2.2)    | 177 ns/iter (+/- 3)       |
| nom        | 9619 ns/iter (+/- 1538)  | 9083 ns/iter (+/- 2193)   |

### After some optimizations

- `cargo bench` for nom
- `-O2 -fllvm` for  attoparsec (using llvm 3.4)
- `-O2` for cereal (apparently, using llvm makes it slower, around 250 ns per iteration)
- Building hammer with `scons --variant=opt`, adding `-Ofast -DNDEBUG` and removing `-g` to the gcc line

|            | small.mp4 (375 kB)       | bigbuckbunny.mp4 (5.3 MB) |
| ---------- | ------------------------ | ------------------------- |
| hammer     | 32424 ns/iter            | 26523 ns/iter             |
| attoparsec | 1138 ns/iter (+/- 55.2)  | 1124 ns/iter (+/- 62.3)   |
| cereal     | 184 ns/iter (+/- 3.2)    | 181 ns/iter (+/- 3.4)     |
| nom        | 240 ns/iter (+/- 56)     | 195 ns/iter (+/- 69)      |


