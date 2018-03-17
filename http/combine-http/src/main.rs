#![feature(test)]
extern crate test;

#[macro_use]
extern crate combine;

use std::{env, fmt};
use std::fs::File;

use combine::{many, token, ParseError, Parser, RangeStream, many1};
use combine::range::{range, take_while1};
use combine::stream::easy;

#[derive(Debug)]
struct Request<'a> {
    method: &'a [u8],
    uri: &'a [u8],
    version: &'a [u8],
}

#[derive(Debug)]
struct Header<'a> {
    name: &'a [u8],
    value: Vec<&'a [u8]>,
}

fn is_token(c: u8) -> bool {
    match c {
        128...255
        | 0...31
        | b'('
        | b')'
        | b'<'
        | b'>'
        | b'@'
        | b','
        | b';'
        | b':'
        | b'\\'
        | b'"'
        | b'/'
        | b'['
        | b']'
        | b'?'
        | b'='
        | b'{'
        | b'}'
        | b' ' => false,
        _ => true,
    }
}

fn is_horizontal_space(c: u8) -> bool {
    c == b' ' || c == b'\t'
}
fn is_space(c: u8) -> bool {
    c == b' '
}
fn is_not_space(c: u8) -> bool {
    c != b' '
}
fn is_http_version(c: u8) -> bool {
    c >= b'0' && c <= b'9' || c == b'.'
}

fn parse_http_request<'a, I>(input: I) -> Result<((Request<'a>, Vec<Header<'a>>), I), I::Error>
where
    I: RangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // Making a closure, because parser instances cannot be reused
    let end_of_line = || (token(b'\r'), token(b'\n')).map(|_| b'\r').or(token(b'\n'));

    let http_version = range(&b"HTTP/"[..]).with(take_while1(is_http_version));

    let request_line = struct_parser!(Request {
            method: take_while1(is_token),
            _: take_while1(is_space),
            uri: take_while1(is_not_space),
            _: take_while1(is_space),
            version: http_version,
        });

    let message_header_line = (
        take_while1(is_horizontal_space),
        take_while1(|c| c != b'\r' && c != b'\n'),
        end_of_line(),
    ).map(|(_, line, _)| line);

    let message_header = (
        take_while1(is_token),
        token(b':'),
        many1(message_header_line),
    ).map(|(name, _, value)| Header {
        name: name,
        value: value,
    });

    let mut request = (
        request_line,
        end_of_line(),
        many(message_header),
        end_of_line(),
    ).map(|(request, _, headers, _)| (request, headers));

    request.parse(input)
}

/*
static REQUESTS: &'static [u8] = include_bytes!("http-requests.txt");

fn http_requests_small(b: &mut Bencher) {
    http_requests_bench(b, easy::Stream(REQUESTS))
}

fn http_requests_large(b: &mut Bencher) {
    use std::iter;

    let mut buffer = Vec::with_capacity(REQUESTS.len() * 5);
    for buf in iter::repeat(REQUESTS).take(5) {
        buffer.extend_from_slice(buf);
    }
    http_requests_bench(b, easy::Stream(&buffer[..]))
}

fn http_requests_large_cheap_error(b: &mut Bencher) {
    use std::iter;

    let mut buffer = Vec::with_capacity(REQUESTS.len() * 5);
    for buf in iter::repeat(REQUESTS).take(5) {
        buffer.extend_from_slice(buf);
    }
    http_requests_bench(b, &buffer[..])
}

fn http_requests_bench<'a, I>(b: &mut Bencher, buffer: I)
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + Clone,
    I::Error: ParseError<I::Item, I::Range, I::Position> + fmt::Debug,
{
    b.iter(|| {
        let mut buf = black_box(buffer.clone());

        while buf.clone().uncons().is_ok() {
            match parse_http_request(buf) {
                Ok(((_, _), b)) => {
                    buf = b;
                }
                Err(err) => panic!("{:?}", err),
            }
        }
    });
}
*/

fn parse(data: &[u8]) -> Option<Vec<(Request, Vec<Header>)>> {
  let mut buf = &data[..];
  let mut v = Vec::new();

  loop {
    match parse_http_request(buf) {
      Ok((r, b)) => {
        buf = b;
        v.push(r);

        if b.is_empty() {
          break;
        }
      }
      Err(err) => return None
    }
  }

  Some(v)
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
  let data = include_bytes!("../../bigger.txt");
  b.iter(||{
    parse(data)
  });
}

#[bench]
fn one_test(b: &mut Bencher) {
  let data = &b"GET / HTTP/1.1
Host: www.reddit.com
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) Gecko/20100101 Firefox/15.0.1
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive"[..];
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
    loop { parse(buf); }
}

