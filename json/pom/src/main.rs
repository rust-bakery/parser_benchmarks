#[macro_use]
extern crate bencher;

extern crate fnv;
extern crate jemallocator;
extern crate pom;

use std::char::{self, REPLACEMENT_CHARACTER};
use std::str::{self, FromStr};

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
use pom::char_class::{digit, hex_digit, multispace};
use pom::parser::*;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

fn space<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).repeat(0..).discard()
}

fn number<'a>() -> Parser<'a, u8, f64> {
    let integer = one_of(b"123456789") - is_a(digit).repeat(0..) | sym(b'0');
    let frac = sym(b'.') + is_a(digit).repeat(1..);
    let exp = one_of(b"eE") + one_of(b"+-").opt() + is_a(digit).repeat(1..);
    let number = sym(b'-').opt() + integer + frac.opt() + exp.opt();
    number
        .collect()
        .convert(str::from_utf8)
        .convert(f64::from_str)
}

fn string<'a>() -> Parser<'a, u8, String> {
    let special_char = one_of(b"\\/\"")
        | sym(b'b').map(|_| b'\x08')
        | sym(b'f').map(|_| b'\x0C')
        | sym(b'n').map(|_| b'\n')
        | sym(b'r').map(|_| b'\r')
        | sym(b't').map(|_| b'\t');
    let escape_sequence = sym(b'\\') * special_char;
    let char_string = (none_of(b"\\\"") | escape_sequence)
        .repeat(1..)
        .convert(String::from_utf8);
    let utf16_char = seq(b"\\u")
        * is_a(hex_digit)
            .repeat(4)
            .convert(String::from_utf8)
            .convert(|digits| u16::from_str_radix(&digits, 16));
    let utf16_string = utf16_char.repeat(1..).map(|chars| {
        char::decode_utf16(chars)
            .map(|r| r.unwrap_or(REPLACEMENT_CHARACTER))
            .collect::<String>()
    });
    let string = sym(b'"') * (char_string | utf16_string).repeat(0..) - sym(b'"');
    string.map(|strings| strings.concat())
}

fn array<'a>() -> Parser<'a, u8, Vec<JsonValue>> {
    let elems = list(call(value), sym(b',') * space());
    sym(b'[') * space() * elems - sym(b']')
}

fn object<'a>() -> Parser<'a, u8, HashMap<String, JsonValue>> {
    let member = string() - space() - sym(b':') - space() + call(value);
    let members = list(member, sym(b',') * space());
    let obj = sym(b'{') * space() * members - sym(b'}');
    obj.map(|members| members.into_iter().collect())
}

fn value<'a>() -> Parser<'a, u8, JsonValue> {
    (seq(b"null").map(|_| JsonValue::Null)
        | seq(b"true").map(|_| JsonValue::Bool(true))
        | seq(b"false").map(|_| JsonValue::Bool(false))
        | number().map(|num| JsonValue::Num(num))
        | string().map(|text| JsonValue::Str(text))
        | array().map(|arr| JsonValue::Array(arr))
        | object().map(|obj| JsonValue::Object(obj)))
        - space()
}

pub fn root<'a>() -> Parser<'a, u8, JsonValue> {
    space() * value() - end()
}

#[test]
fn test() {
    let data = include_bytes!("../../test.json");
    println!("test: {:?}", root().parse(data).unwrap());
    panic!()
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

fn apache(b: &mut Bencher) {
    let data = include_bytes!("../../apache_builds.json");
    b.bytes = data.len() as u64;
    parse(b, data)
}

fn parse<'a>(b: &mut Bencher, buffer: &'a [u8]) {
    b.iter(|| {
        let buf = black_box(buffer);
        match root().parse(buf) {
            Ok(out) => return out,
            Err(err) => panic!("got err: {:?}", err),
        }
    });
}

benchmark_group!(json, basic, data, apache, canada);
benchmark_main!(json);
