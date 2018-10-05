#[macro_use]
extern crate bencher;

#[macro_use]
extern crate combine;

extern crate fnv;

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
use std::hash::Hash;
use std::str;

use combine::error::ParseError;
use combine::{Parser, RangeStream, StreamOnce};

use combine::parser::byte::{byte, spaces};
use combine::parser::choice::{choice, optional};
use combine::parser::combinator::no_partial;
use combine::parser::item::{one_of, satisfy_map};
use combine::parser::range;
use combine::parser::repeat::{escaped, sep_by};
use combine::parser::sequence::between;

#[derive(PartialEq, Debug)]
enum Value<S>
where
    S: Eq + Hash,
{
    Number(f64),
    String(S),
    Bool(bool),
    Null,
    Object(HashMap<S, Value<S>>),
    Array(Vec<Value<S>>),
}

fn lex<'a, P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: RangeStream<Item = u8, Range = &'a [u8]>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    no_partial(p.skip(range::take_while(|b| {
        b == b' ' || b == b'\t' || b == b'\r' || b == b'\n'
    })))
}

fn digits<'a, I>() -> impl Parser<Input = I, Output = &'a [u8]> + 'a
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    range::take_while1(|b| b >= b'0' && b <= b'9')
}

fn number<'a, I>() -> impl Parser<Input = I, Output = f64> + 'a
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    no_partial(
        lex(range::recognize(no_partial((
            optional(one_of("+-".bytes())),
            byte(b'0').or((digits(), optional((byte(b'.'), digits()))).map(|_| b'0')),
            optional((
                (one_of("eE".bytes()), optional(one_of("+-".bytes()))),
                digits(),
            )),
        )))).map(|s: &'a [u8]| str::from_utf8(s).unwrap().parse().unwrap())
            .expected("number"),
    )
}

fn json_string<'a, I>() -> impl Parser<Input = I, Output = &'a str>
where
    I: RangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let back_slash_byte = satisfy_map(|c| {
        Some(match c {
            b'"' => b'"',
            b'\\' => b'\\',
            b'/' => b'/',
            b'b' => '\u{0008}' as u8,
            b'f' => '\u{000c}' as u8,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            _ => return None,
        })
    });
    let inner = range::recognize(escaped(
        range::take_while1(|b| b != b'\\' && b != b'"'),
        b'\\',
        back_slash_byte,
    )).map(|s| str::from_utf8(s).unwrap());
    between(byte(b'"'), lex(byte(b'"')), inner).expected("string")
}

fn object<'a, I>() -> impl Parser<Input = I, Output = HashMap<&'a str, Value<&'a str>>>
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let field = (json_string(), lex(byte(b':')), json_value_()).map(|t| (t.0, t.2));
    let fields = sep_by(field, lex(byte(b',')));
    between(lex(byte(b'{')), lex(byte(b'}')), fields).expected("object")
}

fn array<'a, I>() -> impl Parser<Input = I, Output = Vec<Value<&'a str>>>
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        lex(byte(b'[')),
        lex(byte(b']')),
        sep_by(json_value_(), lex(byte(b','))),
    ).expected("array")
}

#[inline(always)]
fn json_value<'a, I>() -> impl Parser<Input = I, Output = Value<&'a str>>
where
    I: RangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().with(json_value_())
}

// We need to use `parser!` to break the recursive use of `value` to prevent the returned parser
// from containing itself
parser!{
    #[inline(always)]
    fn json_value_['a, I]()(I) -> Value<&'a str>
        where [ I: RangeStream<Item = u8, Range = &'a [u8]> + 'a ]
    {
        choice((
            json_string().map(Value::String),
            object().map(Value::Object),
            array().map(Value::Array),
            number().map(Value::Number),
            lex(range::range(&b"false"[..]).map(|_| Value::Bool(false))),
            lex(range::range(&b"true"[..]).map(|_| Value::Bool(true))),
            lex(range::range(&b"null"[..]).map(|_| Value::Null)),
        ))
    }
}

#[test]
fn json_test() {
    use self::Value::*;
    let input = r#"{
    "array": [1, ""],
    "object": {},
    "number": 3.14,
    "small_number": 0.59,
    "int": -100,
    "exp": -1e2,
    "exp_neg": 23e-2,
    "true": true,
    "false"  : false,
    "null" : null
}"#;
    let result = json_value().easy_parse(input);
    let expected = Object(
        vec![
            ("array", Array(vec![Number(1.0), String("".to_string())])),
            ("object", Object(HashMap::default())),
            ("number", Number(3.14)),
            ("small_number", Number(0.59)),
            ("int", Number(-100.)),
            ("exp", Number(-1e2)),
            ("exp_neg", Number(23E-2)),
            ("true", Bool(true)),
            ("false", Bool(false)),
            ("null", Null),
        ].into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect(),
    );
    match result {
        Ok(result) => assert_eq!(result, (expected, "")),
        Err(e) => {
            println!("{}", e);
            assert!(false);
        }
    }
}

fn parse(b: &mut Bencher, buffer: &str) {
    let mut parser = json_value();
    b.iter(|| {
        let buf = black_box(buffer.as_bytes());

        let result = parser.easy_parse(buf).unwrap();
        black_box(result)
    });
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

#[test]
fn test() {
    let data = "  { \"a\"\t: 42,
  \"b\": [ \"x\", \"y\", 12 ] ,
  \"c\": { \"hello\" : \"world\"
  }
  }  ";
    //let data = include_str!("../../test.json");

    let mut parser = json_value();
    println!("test: {:?}", parser.parse(data).unwrap());
    panic!()
}

fn apache(b: &mut Bencher) {
    let data = include_str!("../../apache_builds.json");
    b.bytes = data.len() as u64;
    parse(b, data)
}

//deactivating the "basic" benchmark because the parser fails on this one
//benchmark_group!(json, basic, data, apache, canada);
benchmark_group!(json, basic, data, apache, canada);
benchmark_main!(json);

/*
fn main() {
  loop {
    let data = include_bytes!("../../canada.json");
    root(data).unwrap();
  }
}
*/
