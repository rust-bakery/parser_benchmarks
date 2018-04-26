# HTTP parsing benchmarks

This repository holds three different versions of a HTTP parser, written in:

- Rust, with [Geal/nom](https://github.com/Geal/nom)
- Rust, with [Marwes/combine](https://github.com/Marwes/combine)
- Rust, with [seanmonstar/httparse](https://github.com/seanmonstar/httparse)
- Haskell with [bos/attoparsec](https://github.com/bos/attoparsec) and [GaloisInc/cereal](https://github.com/GaloisInc/cereal)
- C with [joyent/http-parser](https://github.com/joyent/http-parser)
- C with [h2o/picohttpparser](https://github.com/h2o/picohttpparser)

## Results

The benchmarks were run on a late 2013 Macbook Pro, quad core 2,3 GHz Intel Core i7.

|            | one_test |           small_test          |           bigger_test          | httparse_example_test |
| ---------- | -------- | ----------------------------- | ------------------------------ | --------------------- |
| http-parser  | 606 ns/iter (+/- 367 ns) | 56,385 ns/iter (+/- 148 ns)  | 266254 ns/iter (+/- 37071 ns) | 1137 ns/iter (+/- 18 ns) |
| attoparsec |          | 241.5 μs/iter (+/-5.7 μs)     | 1.836 ms/iter (+/- 137 μs)     |                       |
| combine    | 1,394 ns/iter (+/- 489) = 208 MB/s | 89,875 ns/iter (+/- 12,575) = 237 MB/s | 453,297 ns/iter (+/- 260,973) = 235 MB/s | 2,076 ns/iter (+/- 436) = 338 MB/s |
| httparse   | 213 ns/iter (+/- 39) = 1366 MB/s |   |   | 472 ns/iter (+/- 162) = 1489 MB/s |
| nom | 889 ns/iter (+/- 411) = 327 MB/s | 59,918 ns/iter (+/- 5,372) = 356 MB/s | 333,344 ns/iter (+/- 49,823) = 320 MB/s | 1,544 ns/iter (+/- 294) = 455 MB/s |
| nom (optimized) | 183 ns/iter (+/- 25) = 1590 MB/s | **12,253 ns/iter (+/- 2,473) = 1744 MB/s** | **61,519 ns/iter (+/- 14,660) = 1737 MB/s** | **308 ns/iter (+/- 48) = 2282 MB/s** |
| picohttpparser | **170 ns/iter (+/- 29) = 1711 MB/s** |   |   | 350 ns/iter (+/- 76) = 2008 MB/s |

(the "nom-optimized" version is an experiment to match httparse and picohttparser APIs, and bring
in SIMD based parsing in nom)
