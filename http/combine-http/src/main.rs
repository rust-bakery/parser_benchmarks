#![feature(const_fn)]
#![feature(cfg_target_feature, target_feature, stdsimd)]

#[macro_use]
extern crate bencher;
#[macro_use]
extern crate combine;

use bencher::{black_box, Bencher};

use combine::{many, token, ParseError, parser, Parser, RangeStream, many1, skip_many};
use combine::range::{range, take, take_while1};
use combine::stream::{FullRangeStream, easy};
use combine::error::Consumed;


#[path = "../../nom-optimized/src/combinators.rs"]
mod combinators;

use combinators::{is_header_value_token, is_token};

#[derive(Debug)]
struct Request<'a> {
    method: &'a [u8],
    uri: &'a [u8],
    version: &'a [u8],
}

#[derive(Debug, Copy, Clone)]
struct Header<'a> {
    name: &'a [u8],
    value: &'a [u8],
}

fn is_url_token(c: u8) -> bool {
    c > 0x20 && c < 0x7F
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

fn take_while1_simd<'a, I, F>(range: &'static [u8], mut predicate: F) -> impl Parser<Output = &'a [u8], Input = I>
where
    I: FullRangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    F: FnMut(u8) -> bool,

{
    parser(move |input: &mut I| {
        match combinators::take_while1_simd(input.range(), &mut predicate, range) {
            Ok((remaining, value)) => {
                let _ = input.uncons_range(value.len());
                Ok((value, Consumed::Consumed(())))
            }
            Err(()) => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
        }
    })
}

fn end_of_line<'a, I>() -> impl Parser<Output = u8, Input = I>
where
    I: RangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (token(b'\r'), token(b'\n')).map(|_| b'\r').or(token(b'\n'))
}

fn message_header<'a, I>() -> impl Parser<Output = Header<'a>, Input = I>
where
    I: FullRangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    const header_value_range:&[u8] = b"\0\x08\x0A\x1F\x7F\x7F";
    let message_header_line = (
        take_while1(is_horizontal_space),
        take_while1_simd(header_value_range, is_header_value_token),
        end_of_line(),
    ).map(|(_, line, _)| line);

    (
        take_while1(is_token),
        token(b':'),
        message_header_line,
    ).map(|(name, _, value)| Header {
        name: name,
        value: value,
    })
}

fn parse_http_request<'a, I>(input: I, mut headers: &mut [Header<'a>]) -> Result<(Request<'a>, I), I::Error>
where
    I: FullRangeStream<Item = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let http_version = range(&b"HTTP/"[..]).with(take_while1(is_http_version));

    let request_line = struct_parser!(Request {
            method: take_while1(is_token),
            _: take_while1(is_space),
            uri: take_while1_simd(b"\0 \x7F\x7F", is_url_token),
            _: take_while1(is_space),
            version: http_version,
        });

    let mut headers = headers.iter_mut();
    let mut request = (
        request_line,
        end_of_line(),
        skip_many(message_header().map(|header| {
            *headers.next().unwrap() = header;
        })),
        end_of_line(),
    ).map(|(request, _, _, _)| request);

    request.parse(input)
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
        let mut headers = [Header {
            name: &[],
            value: &[],
        }; 16];

        while !buf.is_empty() {
            // Needed for inferrence for many(message_header)
            match parse_http_request(buf, &mut headers) {
                Ok((o, i)) => {
                    v.push(o);

                    buf = i
                }
                Err(err) => panic!("got err: {:?}", err),
            }
        }

        v
    });
}

benchmark_group!(
    http,
    one_test,
    small_test,
    bigger_test,
    httparse_example_test
);
benchmark_main!(http);
