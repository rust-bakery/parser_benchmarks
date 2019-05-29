# JSON parsing benchmarks

these parsers have been extracted from the combine, nom and pest
libraries. Note that these parsers tend to follow the idiomatic
way of their libraries. More optimized versions might exist (and
would make good additions to this repository), but they might
be harder to follow.

## Results

The benchmarks were run on a late 2013 Macbook Pro, quad core 2,3 GHz Intel Core i7. in bold, the best result
of each column. Some parsers work on strings, other on raw byte slices (and need to convert strings to `&str`).


|   &[u8] parsers   | basic                               | canada.json |apache_builds.json | data.json |
| ----------------- | ----------------------------------- | ----------- | ----------------- | --------- |
| serde_json 1.0.32 | 809 ns/iter (+/- 52) = 93 MB/s      | **13,894,038 ns/iter (+/- 891,963) = 162 MB/s** | 518,642 ns/iter (+/- 34,784) = 245 MB/s | 43,749 ns/iter (+/- 22,790) = 211 MB/s |
| nom 4.2.3         | **754 ns/iter (+/- 95) = 100 MB/s** | 38,646,927 ns/iter (+/- 3,008,518) = 58 MB/s | **458,404 ns/iter (+/- 36,534) = 277 MB/s** | **33,483 ns/iter (+/- 2,518) = 276 MB/s** |

|   &str parsers    | basic                             | canada.json |apache_builds.json | data.json |
| ----------------- | --------------------------------- | ----------- | ----------------- | --------- |
| combine 3.5.2     | 1,751 ns/iter (+/- 156) = 43 MB/s | 62,077,520 ns/iter (+/- 5,061,098) = 36 MB/s | 847,121 ns/iter (+/- 103,110) = 150 MB/s | 66,307 ns/iter (+/- 10,838) = 139 MB/s |
| pest 1.0.6        | 1,361 ns/iter (+/- 240) = 55 MB/s | 17,313,027 ns/iter (+/- 1,726,953) = 130 MB/s | 1,449,729 ns/iter (+/- 203,113) = 87 MB/s | 117,027 ns/iter (+/- 13,408) = 79 MB/s |
| peg 0.5.7         | 3,101 ns/iter (+/- 312) = 24 MB/s | 74,108,415 ns/iter (+/- 3,583,293) = 30 MB/s | 1,758,852 ns/iter (+/- 311,128) = 72 MB/s | 131,039 ns/iter (+/- 18,700) = 70 MB/s |
| serde_json 1.0.32 | **638 ns/iter (+/- 61) = 119 MB/s** | **12,367,415 ns/iter (+/- 752,284) = 182 MB/s** | **363,077 ns/iter (+/- 27,709) = 350 MB/s** | **26,234 ns/iter (+/- 4,009) = 352 MB/s** |
| nom 4.2.3         | 1,159 ns/iter (+/- 82) = 65 MB/s | 45,030,200 ns/iter (+/- 2,797,900) = 49 MB/s | 597,444 ns/iter (+/- 39,718) = 213 MB/s | 45,852 ns/iter (+/- 3,691) = 201 MB/s |
