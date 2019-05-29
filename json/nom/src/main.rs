#[macro_use]
extern crate bencher;
#[macro_use]
extern crate nom;

extern crate fnv;

extern crate jemallocator;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
//use nom::{HexDisplay, alphanumeric, recognize_float};
use nom::{
  branch::alt,
  bytes::complete::{escaped, tag, take_while, take_while1, is_a},
  character::complete::{alphanumeric1 as alphanumeric, char, one_of},
  combinator::{map, map_res, opt, cut, iterator},
  multi::separated_list,
  number::complete::double,
  sequence::{delimited, preceded, separated_pair, terminated, pair},
  Err, IResult, HexDisplay
};

use std::str;

pub fn is_string_character(c: u8) -> bool {
  //FIXME: should validate unicode character
  c != b'"' && c != b'\\'
}

pub fn is_space(c: u8) -> bool {
  c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

fn sp(i: &[u8]) -> IResult<&[u8], &[u8]> {
  take_while(is_space)(i)
}

#[derive(Debug, PartialEq)]
pub enum JsonValue<'a> {
  Str(&'a str),
  Boolean(bool),
  Num(f64),
  Array(Vec<JsonValue<'a>>),
  Object(HashMap<&'a str, JsonValue<'a>>),
}

//FIXME: handle the cases like \u1234
fn string(i: &[u8]) -> IResult<&[u8], &str> {
  preceded(
    char('\"'),
    cut(terminated(
        // we could avoid from_utf8 here, since we already confirmed alphanumeric chars
        //map_res(escaped(alphanumeric, '\\', one_of("\"n\\")), str::from_utf8),
        map_res(escaped(take_while1(is_string_character), '\\', one_of("\"bfnrt\\")), str::from_utf8),
        char('\"')
    ))
  )(i)
}

fn boolean(i: &[u8]) -> IResult<&[u8], bool> {
  alt((
      map(tag("false"), |_| false),
      map(tag("true"), |_| true)
  ))(i)
}

fn array(i: &[u8]) -> IResult<&[u8], Vec<JsonValue>> {
  preceded(char('['),
    cut(terminated(
        separated_list(preceded(sp, char(',')), value),
        preceded(sp, char(']'))))
  )(i)
}

fn key_value(i: &[u8]) -> IResult<&[u8], (&str, JsonValue)> {
  separated_pair(preceded(sp, string), cut(preceded(sp, char(':'))), value)(i)
}

fn hash(i: &[u8]) -> IResult<&[u8], HashMap<&str, JsonValue>> {
  let (i, _) = char('{')(i)?;
  let mut res = HashMap::default();

  match key_value(i) {
    Err(_) => {
      preceded(sp, char('}'))(i).map(|(i, _)| (i, res))
    },
    Ok((i, first)) => {
      let mut it = iterator(i, preceded(pair(sp, char(',')), key_value));
      res.extend(&mut it);

      let (i, _) = it.finish()?;
      preceded(sp, char('}'))(i).map(|(i, _)| (i, res))
    }
  }
}

fn value(i: &[u8]) -> IResult<&[u8], JsonValue> {
  preceded(sp,
    alt((
      map(hash, JsonValue::Object),
      map(array, JsonValue::Array),
      map(string, JsonValue::Str),
      map(double, JsonValue::Num),
      map(boolean, JsonValue::Boolean)
    ))
  )(i)
}

fn root(i: &[u8]) -> IResult<&[u8], JsonValue> {
  delimited(
    sp,
    alt((map(hash, JsonValue::Object), map(array, JsonValue::Array))),
    opt(sp)
  )(i)
}

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
        if let &nom::Err::Error((ref i, ref e)) = &err {
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
