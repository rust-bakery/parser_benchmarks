#[macro_use]
extern crate bencher;

#[macro_use]
extern crate nom;

use bencher::{black_box, Bencher};

use nom::IResult;
use std::env;
use std::fs::File;

#[derive(Debug)]
struct Request<'a> {
    method:  &'a [u8],
    uri:     &'a [u8],
    version: u8,
}

#[derive(Debug)]
struct Header<'a> {
    name:  &'a [u8],
    value: Vec<&'a [u8]>,
}

fn is_token(c: u8) -> bool {
  c > 0x20 && c < 0x7F
}

macro_rules! byte_map {
    ($($flag:expr,)*) => ([
        $($flag != 0,)*
    ])
}

static HEADER_NAME_MAP: [bool; 256] = byte_map![
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

#[inline]
fn is_header_name_token(b: u8) -> bool {
    HEADER_NAME_MAP[b as usize]
}

fn not_line_ending(c: u8) -> bool {
    c != b'\r' && c != b'\n'
}

fn is_space(c: u8) -> bool {
    c == b' '
}

fn is_not_space(c: u8)        -> bool { c != b' ' }
fn is_horizontal_space(c: u8) -> bool { c == b' ' || c == b'\t' }

fn is_version(c: u8) -> bool {
    c >= b'0' && c <= b'9' || c == b'.'
}

named!(line_ending, alt!(tag!("\r\n") | tag!("\n")));

fn request_line<'a>(input: &'a [u8]) -> IResult<&'a[u8], Request<'a>> {
  do_parse!(input,
    method: take_while1!(is_token)     >>
            take_while1!(is_space)     >>
    url:    take_while1!(is_not_space) >>
            take_while1!(is_space)     >>
    version: http_version              >>
    line_ending                        >>
    ( Request {
        method: method,
        uri:    url,
        version: version,
    } )
  )
}

named!(http_version<u8>, preceded!(
    tag!("HTTP/1."),
    map!(one_of!("01"), |n| if n == '0' { 0 } else { 1 })
));

named!(message_header_value, delimited!(
    take_while1!(is_horizontal_space),
    take_while1!(not_line_ending),
    line_ending
));

fn message_header<'a>(input: &'a [u8]) -> IResult<&'a[u8], Header<'a>> {
  do_parse!(input,
    name:   take_while1!(is_header_name_token)       >>
            char!(':')                   >>
    values: many1!(message_header_value) >>

    ( Header {
        name: name,
        value: values,
    } )
  )
}

fn request<'a>(input: &'a [u8]) -> IResult<&'a[u8], (Request<'a>, Vec<Header<'a>>)> {
  terminated!(input,
    pair!(request_line, many1!(message_header)),
    line_ending
  )
}

fn small_test(b: &mut Bencher) {
  let data = include_bytes!("../../http-requests.txt");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn bigger_test(b: &mut Bencher) {
  let data = include_bytes!("../../bigger.txt");
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn one_test(b: &mut Bencher) {
  let data = &b"GET / HTTP/1.1
Host: www.reddit.com
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) Gecko/20100101 Firefox/15.0.1
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive

"[..];
  b.bytes = data.len() as u64;
  parse(b, data)
}

fn httparse_example_test(b: &mut Bencher) {
  let data = &b"GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
Host: www.kittyhell.com\r\n\
User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
Accept-Encoding: gzip,deflate\r\n\
Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
Keep-Alive: 115\r\n\
Connection: keep-alive\r\n\
Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n\r\n"[..];

  b.bytes = data.len() as u64;
  parse(b, data)
}

fn parse(b: &mut Bencher, buffer: &[u8]) {
    b.iter(|| {
        let mut buf = black_box(buffer);
        let mut v = Vec::new();

        while !buf.is_empty() {
            match request(buf) {
                Ok((i, o)) => {
                    v.push(o);

                    buf = i
                }
                Err(err) => panic!("got err: {:?}", err),
            }
        }

        v
    });
}

benchmark_group!(http, one_test, small_test, bigger_test, httparse_example_test);
benchmark_main!(http);

/*
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
*/


