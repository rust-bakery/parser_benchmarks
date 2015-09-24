#![feature(test)]
extern crate test;

#[macro_use]
extern crate nom;

use nom::IResult;
use std::env;
use std::fs::File;

#[derive(Debug)]
struct Request<'a> {
    method:  &'a [u8],
    uri:     &'a [u8],
    version: &'a [u8],
}

#[derive(Debug)]
struct Header<'a> {
    name:  &'a [u8],
    value: Vec<&'a [u8]>,
}

fn is_token(c: u8) -> bool {
    // roughly follows the order of ascii chars: "\"(),/:;<=>?@[\\]{} \t"
    c < 128 && c > 32 && c != b'\t' && c != b'"' && c != b'(' && c != b')' &&
        c != b',' && c != b'/' && !(c > 57 && c < 65) && !(c > 90 && c < 94) &&
        c != b'{' && c != b'}'
}

fn not_line_ending(c: u8) -> bool {
    c != b'\r' && c != b'\n'
}

fn is_space(c: u8) -> bool {
    c == b' '
}

fn is_version(c: u8) -> bool {
    c >= b'0' && c <= b'9' || c == b'.'
}

named!(line_ending, alt!(tag!("\n") | tag!("\r\n")));

named!(request_line(&'a [u8]) -> Request<'a>, chain!(
    method: filter!(is_token) ~
    filter!(is_space) ~
    url: take_until_and_consume!(" ") ~
    version: http_version ~
    line_ending,
    
    || Request {
        method: method,
        uri:    url,
        version: version,
    }));

named!(http_version, chain!(
    tag!("HTTP/") ~
    version: filter!(is_version),
    
    || version));

named!(message_header_value, chain!(
    many1!(alt!(tag!(" ") | tag!("\t"))) ~
    data: filter!(not_line_ending) ~
    line_ending,
    
    || data));

named!(message_header(&'a [u8]) -> Header<'a>, chain!(
    name: filter!(is_token) ~
    tag!(":") ~
    values: many1!(message_header_value),
    
    || Header {
        name: name,
        value: values,
    }));

named!(request(&'a [u8]) -> (Request<'a>, Vec<Header<'a>>), chain!(
    req: request_line ~
    h:   many1!(message_header) ~
    line_ending,
    
    || (req, h)));


fn parse(data:&[u8]) {
  let mut buf = &data[..];
  let mut i   = 0;
  loop {
    match request(buf) {
      IResult::Done(b, _) => {
        buf = b;

        i = i + 1;

        if b.is_empty() {
    
    //println!("{}", i);
          break;
        }
      },
      IResult::Error(e) => return /*panic!("{:?}", e)*/,
      IResult::Incomplete(_) => return /*panic!("Incomplete!")*/,
    }
  }
}

use test::Bencher;
#[bench]
fn small_test(b: &mut Bencher) {
  let data = include_bytes!("../../http-requests.txt");
  b.iter(||{
    parse(data)
  });
}

#[bench]
fn bigger_test(b: &mut Bencher) {
  let data = include_bytes!("../../requests.txt");
  b.iter(||{
    parse(data)
  });
}

fn main() {
    let mut contents: Vec<u8> = Vec::new();

    {
        use std::io::Read;

        let mut file = File::open(env::args().nth(1).expect("File to read")).ok().expect("Failed to open file");

        let _ = file.read_to_end(&mut contents).unwrap();
    }
    
    let mut buf = &contents[..];
    loop { parse(buf) }
}

