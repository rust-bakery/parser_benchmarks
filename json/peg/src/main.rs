#[macro_use]
extern crate bencher;

extern crate fnv;

use fnv::FnvHashMap as HashMap;
use bencher::{black_box, Bencher};

mod peg_json {
    include!(concat!(env!("OUT_DIR"), "/json.rs"));
}

#[derive(Debug, PartialEq)]
pub enum JsonValue<'a> {
  Str(&'a str),
  Num(f64),
  Boolean(bool),
  Array(Vec<JsonValue<'a>>),
  Object(HashMap<&'a str, JsonValue<'a>>),
}

fn basic(b: &mut Bencher) {
  let data = "  { \"a\"\t: 42,
  \"b\": [ \"x\", \"y\", 12 ] ,
  \"c\": { \"hello\" : \"world\"
  }
  }  ";

  b.bytes = data.len() as u64;
  parse(b, data)
}

fn apache(b: &mut Bencher) {
  let data = include_str!("../../apache_builds.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn data(b: &mut Bencher) {
  let data = include_str!("../../data.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn canada(b: &mut Bencher) {
  let data = include_str!("../../canada.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

// #[test]
// fn test() {
//   let data = include_str!("../../test.json");
//   println!("test: {:?}", peg_json::root(data).unwrap());
//   panic!()
// }

fn parse<'a>(b: &mut Bencher, buffer: &'a str) {
  let buf = black_box(buffer);
  b.iter(|| {
    match peg_json::root(buf) {
      Ok(o) => o,
      Err(err) => {
        panic!("got err: {:?}", err)
      }
    }
  });
}

benchmark_group!(json, basic, data, apache, canada);
benchmark_main!(json);
