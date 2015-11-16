# Nom benchmarks

This repository holds three different versions of a HTTP parser, written in:

- Rust, with [Geal/nom](https://github.com/Geal/nom)
- Haskell with [bos/attoparsec](https://github.com/bos/attoparsec) and [GaloisInc/cereal](https://github.com/GaloisInc/cereal)
- C with [joyent/http-parser](https://github.com/joyent/http-parser)

## Some benchmark results

The benchmarks were run on a late 2013 Macbook Pro, quad core 2,3 GHz Intel Core i7.

|            |           21kB                |           104kB                |
| ---------- | ----------------------------- | ------------------------------ |
| manual C   | 62,451 ns/iter (+/- 1000 ns)  | 300,000 ns/iter (+/- 16 ns)    |
| nom        | 48,420 ns/iter (+/- 2,662 ns) | 250,547 ns/iter (+/- 6,967 ns) |
| attoparsec | 241.5 μs/iter (+/-5.7 μs)     | 1.836 ms/iter (+/- 137 μs)     |
