extern crate pest;
extern crate pest_grammars;

#[macro_use]
extern crate bencher;

extern crate fnv;

use bencher::{black_box, Bencher};

use pest::Parser;
use pest::Span;
use pest::iterators::Pair;

use pest_grammars::json::*;

use fnv::FnvHashMap as HashMap;

enum Json<'i> {
    Null,
    Bool(bool),
    Number(f64),
    String(Span<'i>),
    Array(Vec<Json<'i>>),
    Object(HashMap<Span<'i>, Json<'i>>)
}

fn consume(pair: Pair<Rule>) -> Json {
    fn value(pair: Pair<Rule>) -> Json {
        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::null => Json::Null,
            Rule::bool => match pair.as_str() {
                "false" => Json::Bool(false),
                "true" => Json::Bool(true),
                _ => unreachable!()
            },
            Rule::number => Json::Number(pair.as_str().parse().unwrap()),
            Rule::string => Json::String(pair.into_span()),
            Rule::array => Json::Array(pair.into_inner().map(value).collect()),
            Rule::object => {
                let pairs = pair.into_inner().map(|pos| {
                    let mut pair = pos.into_inner();

                    let key = pair.next().unwrap().into_span();
                    let value = value(pair.next().unwrap());

                    (key, value)
                });

                Json::Object(pairs.collect())
            }
            _ => unreachable!()
        }
    }

    value(pair)
}

fn basic(b: &mut Bencher) {
  let data = &"  { \"a\"\t: 42,
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

fn apache(b: &mut Bencher) {
  let data = include_str!("../../apache_builds.json");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn parse(b: &mut Bencher, buffer: &str) {
  b.iter(|| {
    let mut buf = black_box(buffer);
    JsonParser::parse(Rule::json, buf).unwrap()
  })
}

benchmark_group!(json, basic, data, canada, apache);
benchmark_main!(json);

/*
fn main() {
  let data = include_str!("../../canada.json");
  loop {
    JsonParser::parse(Rule::json, data).unwrap();
  }
}
*/
