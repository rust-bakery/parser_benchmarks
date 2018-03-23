# JSON parsing benchmarks

these parsers have been extracted from the combine, nom and pest
libraries. Note that these parsers tend to follow the idiomatic
way of their libraries. More optimized versions might exist (and
would make good additions to this repository), but they might
be harder to follow.

## Results

The benchmarks were run on a late 2013 Macbook Pro, quad core 2,3 GHz Intel Core i7.

|         | basic                             | canada.json |apache_builds.json | data.json |
| ------- | --------------------------------- | ----------- | ----------------- | --------- |
| combine | (fails)                           | 127,775,522 ns/iter (+/- 11,140,676) = 17 MB/s | 3,732,534 ns/iter (+/- 795,836) = 34 MB/s | 241,407 ns/iter (+/- 40,575) = 38 MB/s |
| nom     | **1,333 ns/iter (+/- 247) = 57 MB/s** | 62,971,567 ns/iter (+/- 6,311,768) = 35 MB/s   | **1,209,550 ns/iter (+/- 323,936) = 105 MB/s** | **62,008 ns/iter (+/- 11,685) = 149 MB/s** |
| pest    | 1,405 ns/iter (+/- 238) = 54 MB/s | **27,701,820 ns/iter (+/- 3,961,221) = 81 MB/s**   | 1,694,463 ns/iter (+/- 338,194) = 75 MB/s | 131,851 ns/iter (+/- 22,667) = 70 MB/s |
