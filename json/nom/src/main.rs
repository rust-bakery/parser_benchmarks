#[macro_use]
extern crate bencher;
#[macro_use]
extern crate nom;

extern crate fnv;

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
use nom::{HexDisplay, alphanumeric, recognize_float};

use std::str;

pub fn is_string_character(c: u8) -> bool {
  //FIXME: should validate unicode character
  c != b'"' && c != b'\\'
}

pub fn is_space(c: u8) -> bool {
  c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

named!(sp, take_while!(is_space));

#[derive(Debug, PartialEq)]
pub enum JsonValue<'a> {
  Str(&'a str),
  Boolean(bool),
  Num(f64),
  Array(Vec<JsonValue<'a>>),
  Object(HashMap<&'a str, JsonValue<'a>>),
}

named!(float<f64>, flat_map!(recognize_float, parse_to!(f64)));

//FIXME: handle the cases like \u1234
named!(
  string<&str>,
  delimited!(
    char!('\"'),
    map_res!(
      escaped!(take_while1!(is_string_character), '\\', one_of!("\"bfnrt\\")),
      str::from_utf8
    ),
    char!('\"')
  )
);

named!(
  boolean<bool>,
  alt!(value!(false, tag!("false")) | value!(true, tag!("true")))
);

named!(
  array<Vec<JsonValue>>,
  delimited!(
    char!('['),
    return_error!(separated_list!(preceded!(sp, char!(',')), value)),
    preceded!(sp, char!(']'))
  )
);

named!(
  key_value<(&str, JsonValue)>,
  separated_pair!(ws!(string), char!(':'), value)
);

named!(
  hash<HashMap<&str, JsonValue>>,
  map!(
    delimited!(
      char!('{'),
      return_error!(separated_list!(preceded!(sp, char!(',')), key_value)),
      preceded!(sp, char!('}'))
    ),
    |tuple_vec| tuple_vec
      .into_iter()
      .collect()
  )
);

named!(
  value<JsonValue>,
  preceded!(sp, alt!(
    map!(string, JsonValue::Str)  |
    map!(float, JsonValue::Num)   |
    map!(array, JsonValue::Array) |
    map!(hash, JsonValue::Object) |
    map!(boolean, JsonValue::Boolean)
  ))
);

named!(
  root<JsonValue>,
  delimited!(
    call!(sp),
    alt!(
      map!(hash, JsonValue::Object) |
      map!(array, JsonValue::Array)
    ),
    not!(complete!(sp))
  )
);

fn basic(b: &mut Bencher) {
  let data = b"  { \"a\"\t: 42,
  \"b\": [ \"x\", \"y\", 12 ] ,
  \"c\": { \"hello\" : \"world\"
  }
  }  ";

  b.bytes = data.len() as u64;
  parse(b, &data[..])
}

fn data(b: &mut Bencher) {
  let data = include_bytes!("../../data.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn canada(b: &mut Bencher) {
  let data = include_bytes!("../../canada.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

#[test]
fn test() {
  let data = include_bytes!("../../test.json");
  println!("test: {:?}", root(data).unwrap());
  panic!()
}

fn apache(b: &mut Bencher) {
  let data = include_bytes!("../../apache_builds.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn parse<'a>(b: &mut Bencher, buffer: &'a[u8]) {
  b.iter(|| {
    let mut buf = black_box(buffer);
    match root(buf) {
      Ok((i, o)) => {
        return o;
      }
      Err(err) => {
        if let &nom::Err::Error(nom::Context::Code(ref i, ref e)) = &err {
          panic!("got err {:?} at:\n{}", e, i.to_hex(16));
        } else {
          panic!("got err: {:?}", err)
        }
      },
    }
  });
}


//benchmark_group!(json, basic, data, apache);
benchmark_group!(json, basic, data, apache, canada);
benchmark_main!(json);

/*
fn main() {
  loop {
    let data = include_bytes!("../../data.json");
    root(data).unwrap();
  }
}
*/
