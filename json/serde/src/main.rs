#[macro_use]
extern crate bencher;

extern crate fnv;
extern crate serde;
extern crate serde_json;

use std::borrow::Cow;
use std::fmt;

use bencher::{black_box, Bencher};
use fnv::FnvHashMap as HashMap;
use serde::de::{Deserialize, Deserializer, MapAccess, SeqAccess, Visitor};

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Str(Cow<'a, str>),
    Num(f64),
    Boolean(bool),
    Array(Vec<Value<'a>>),
    Object(HashMap<&'a str, Value<'a>>),
}

impl<'de: 'a, 'a> Deserialize<'de> for Value<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value<'de>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("any JSON value")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
                Ok(Value::Str(Cow::Owned(v.to_owned())))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E> {
                Ok(Value::Str(Cow::Borrowed(v)))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> {
                Ok(Value::Num(v as f64))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E> {
                Ok(Value::Num(v as f64))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> {
                Ok(Value::Num(v))
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E> {
                Ok(Value::Boolean(v))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut array = Vec::new();
                while let Some(value) = seq.next_element()? {
                    array.push(value);
                }
                Ok(Value::Array(array))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut object = HashMap::default();
                while let Some((key, value)) = map.next_entry()? {
                    object.insert(key, value);
                }
                Ok(Value::Object(object))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
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

fn apache(b: &mut Bencher) {
    let data = include_str!("../../apache_builds.json");
    b.bytes = data.len() as u64;
    parse(b, data)
}

fn parse(b: &mut Bencher, buffer: &str) {
    b.iter(|| {
        serde_json::from_str::<Value>(black_box(buffer)).unwrap()
    });
}

benchmark_group!(json, basic, data, apache, canada);
benchmark_main!(json);
