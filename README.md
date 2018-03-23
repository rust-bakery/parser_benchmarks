# parser benchmarks

This repository holds benchmarks comparing various parser libraries:

- [nom](https://github.com/Geal/nom), a parser combinators library written in Rust
- [combine](https://github.com/Marwes/combine), a parser combinators library written in Rust
- [pest](https://github.com/pest-parser/pest) a PEG parser written in Rust
- [hammer](https://github.com/UpstandingHackers/hammer) a parsers combinators library written in C
- [attoparsec](https://hackage.haskell.org/package/attoparsec), a parsers combinators library written in Haskell

Along with existing libraries for comparison:

- [Joyent's HTTP parser for NodeJS](https://github.com/nodejs/http-parser), written in C
- [picohttpparser](https://github.com/h2o/picohttpparser), written in C
- [httparse](https://github.com/seanmonstar/httparse), written in Rust

Right now, there are 3 examples:
- HTTP request header parsing
- JSON parsing
- MP4 filetype atom parsing

The goal of this repository is to gather various parsing solutions and compare them
on performance, but also usability (testing how easy it is to write and maintain
a parser for complex, real world formats).
It is also a good testing ground to see if some techniques might be transferrable from
one parsing solution to another.
