#![feature(conservative_impl_trait)]

#[macro_use]
extern crate bencher;

#[macro_use]
extern crate combine;

use std::collections::HashMap;
use std::hash::Hash;
use bencher::{black_box, Bencher};

use combine::{Parser, RangeStream, Stream, StreamOnce};
use combine::error::{Consumed, ParseError};

use combine::parser::char::{char, digit, spaces, string};
use combine::parser::item::{any, one_of, satisfy_map};
use combine::parser::sequence::between;
use combine::parser::repeat::{sep_by, skip_many, skip_many1};
use combine::parser::choice::{choice, optional};
use combine::parser::function::parser;
use combine::parser::range;

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

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn number<'a, I>() -> impl Parser<Input = I, Output = f64>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(range::recognize((
        optional(char('-')),
        char('0').or((
            skip_many1(digit()),
            optional((char('.'), skip_many(digit()))),
        ).map(|_| '0')),
        optional((
            (one_of("eE".chars()), optional(one_of("+-".chars()))),
            skip_many1(digit()),
        )),
    ))).map(|s: &'a str| s.parse().unwrap())
        .expected("number")
}

fn json_char<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (c, consumed) = try!(any().parse_lazy(input).into());
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });
        match c {
            '\\' => consumed.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, consumed)),
        }
    })
}

fn json_string<'a, I>() -> impl Parser<Input = I, Output = &'a str>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        char('"'),
        lex(char('"')),
        range::recognize(skip_many(json_char())),
    ).expected("string")
}

fn object<'a, I>() -> impl Parser<Input = I, Output = HashMap<&'a str, Value<&'a str>>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let field = (json_string(), lex(char(':')), json_value_()).map(|t| (t.0, t.2));
    let fields = sep_by(field, lex(char(',')));
    between(lex(char('{')), lex(char('}')), fields).expected("object")
}

fn array<'a, I>() -> impl Parser<Input = I, Output = Vec<Value<&'a str>>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        lex(char('[')),
        lex(char(']')),
        sep_by(json_value_(), lex(char(','))),
    ).expected("array")
}

#[inline(always)]
fn json_value<'a, I>() -> impl Parser<Input = I, Output = Value<&'a str>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().with(json_value_())
}

// We need to use `parser!` to break the recursive use of `value` to prevent the returned parser
// from containing itself
parser!{
    #[inline(always)]
    fn json_value_['a, I]()(I) -> Value<I::Range>
        where [ I: RangeStream<Item = char, Range = &'a str> ]
    {
        choice((
            json_string().map(Value::String),
            object().map(Value::Object),
            array().map(Value::Array),
            number().map(Value::Number),
            lex(string("false").map(|_| Value::Bool(false))),
            lex(string("true").map(|_| Value::Bool(true))),
            lex(string("null").map(|_| Value::Null)),
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
            ("object", Object(HashMap::new())),
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
        let buf = black_box(buffer);

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
