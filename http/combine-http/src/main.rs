#[macro_use]
extern crate bencher;
#[macro_use]
extern crate combine;

use bencher::{black_box, Bencher};

use combine::parser::combinator::no_partial;
use combine::range::{range, take_while1};
use combine::stream::FullRangeStream;
use combine::{many, one_of, token, ParseError, Parser, RangeStream};

#[derive(Debug)]
struct Request<'a> {
    method: &'a [u8],
    uri: &'a [u8],
    version: u8,
}

#[derive(Debug, Copy, Clone)]
struct Header<'a> {
    name: &'a [u8],
    value: &'a [u8],
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

fn is_header_value_token(c: u8) -> bool {
    return c == '\t' as u8 || (c > 31 && c != 127);
}

fn is_url_token(c: u8) -> bool {
    c > 0x20 && c < 0x7F
}

fn is_horizontal_space(c: u8) -> bool {
    c == b' ' || c == b'\t'
}

fn end_of_line<'a, I>() -> impl Parser<Output = (), Input = I> + 'a
where
    I: RangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    range(b"\r\n" as &[u8])
        .or(range(b"\n" as &[u8]))
        .map(|_| ())
}

fn message_header<'a, I>() -> impl Parser<Output = Header<'a>, Input = I>
where
    I: FullRangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let header_value = no_partial((
        take_while1(is_horizontal_space),
        take_while1(is_header_value_token),
        end_of_line(),
    )).map(|(_, line, _)| line);

    no_partial((take_while1(is_token), token(b':'), header_value))
        .map(|(name, _, value)| Header { name, value })
}

fn parse_http_request<'a, I>(input: I) -> Result<((Request<'a>, Vec<Header<'a>>), I), I::Error>
where
    I: FullRangeStream<Item = u8, Range = &'a [u8]> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let http_version = range(&b"HTTP/1."[..]).with(one_of(b"01".iter().cloned()).map(|c| {
        if c == b'0' {
            0
        } else {
            1
        }
    }));

    let request_line = no_partial(struct_parser!(Request {
            method: take_while1(is_token),
            _: token(b' '),
            uri: take_while1(is_url_token),
            _: token(b' '),
            version: http_version,
        }));

    let mut request = no_partial((
        request_line,
        end_of_line(),
        many(message_header()),
        end_of_line(),
    )).map(|(request, _, headers, _)| (request, headers));

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

        while !buf.is_empty() {
            match parse_http_request(buf) {
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
