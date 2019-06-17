#[macro_use]
extern crate bencher;

extern crate fnv;
extern crate jemallocator;
extern crate pom;

use std::char::{self, REPLACEMENT_CHARACTER};
use std::iter::FromIterator;
use std::str::{self, FromStr};

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
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

fn space<'a>() -> Parser<'a, char, ()> {
    one_of(" \t\r\n").repeat(0..).discard()
}

fn number<'a>() -> Parser<'a, char, f64> {
    let integer = one_of("123456789") - one_of("0123456789").repeat(0..) | sym('0');
    let frac = sym('.') + one_of("0123456789").repeat(1..);
    let exp = one_of("eE") + one_of("+-").opt() + one_of("0123456789").repeat(1..);
    let number = sym('-').opt() + integer + frac.opt() + exp.opt();
    number
        .collect()
        .map(String::from_iter)
        .convert(|s| f64::from_str(&s))
}

fn string<'a>() -> Parser<'a, char, String> {
    let special_char = one_of("\\/\"")
        | sym('b').map(|_| '\x08')
        | sym('f').map(|_| '\x0C')
        | sym('n').map(|_| '\n')
        | sym('r').map(|_| '\r')
        | sym('t').map(|_| '\t');
    let escape_sequence = sym('\\') * special_char;
    let char_string = (none_of("\\\"") | escape_sequence)
        .repeat(1..)
        .map(String::from_iter);
    let utf16_char = tag("\\u")
        * is_a(|c: char| c.is_digit(16))
            .repeat(4)
            .map(String::from_iter)
            .convert(|digits| u16::from_str_radix(&digits, 16));
    let utf16_string = utf16_char.repeat(1..).map(|chars| {
        char::decode_utf16(chars)
            .map(|r| r.unwrap_or(REPLACEMENT_CHARACTER))
            .collect::<String>()
    });
    let string = sym('"') * (char_string | utf16_string).repeat(0..) - sym('"');
    string.map(|strings| strings.concat())
}

fn array<'a>() -> Parser<'a, char, Vec<JsonValue>> {
    let elems = list(call(value), sym(',') * space());
    sym('[') * space() * elems - sym(']')
}

fn object<'a>() -> Parser<'a, char, HashMap<String, JsonValue>> {
    let member = string() - space() - sym(':') - space() + call(value);
    let members = list(member, sym(',') * space());
    let obj = sym('{') * space() * members - sym('}');
    obj.map(|members| members.into_iter().collect())
}

fn value<'a>() -> Parser<'a, char, JsonValue> {
    (tag("null").map(|_| JsonValue::Null)
        | tag("true").map(|_| JsonValue::Bool(true))
        | tag("false").map(|_| JsonValue::Bool(false))
        | number().map(|num| JsonValue::Num(num))
        | string().map(|text| JsonValue::Str(text))
        | array().map(|arr| JsonValue::Array(arr))
        | object().map(|obj| JsonValue::Object(obj)))
        - space()
}

pub fn root<'a>() -> Parser<'a, char, JsonValue> {
    space() * value() - end()
}

#[test]
fn test() {
    let data = include_str!("../../test.json");
    let chars: Vec<_> = data.chars().collect();
    println!("test: {:?}", root().parse(&chars).unwrap());
    panic!()
}

fn basic(b: &mut Bencher) {
    let data = "  { \"a\"\t: 42,
  \"b\": [ \"x\", \"y\", 12 ] ,
  \"c\": { \"hello\" : \"world\"
  }
  }  ";

    let chars: Vec<_> = data.chars().collect();
    b.bytes = chars.len() as u64;
    parse(b, &chars)
}

fn data(b: &mut Bencher) {
    let data = include_str!("../../data.json");
    let chars: Vec<_> = data.chars().collect();
    b.bytes = chars.len() as u64;
    parse(b, &chars)
}

fn canada(b: &mut Bencher) {
    let data = include_str!("../../canada.json");
    let chars: Vec<_> = data.chars().collect();
    b.bytes = chars.len() as u64;
    parse(b, &chars)
}

fn apache(b: &mut Bencher) {
    let data = include_str!("../../apache_builds.json");
    let chars: Vec<_> = data.chars().collect();
    b.bytes = chars.len() as u64;
    parse(b, &chars)
}

fn parse<'a>(b: &mut Bencher, buffer: &'a [char]) {
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
