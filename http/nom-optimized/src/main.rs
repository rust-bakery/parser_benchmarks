#![feature(const_fn)]
#![feature(cfg_target_feature, target_feature, stdsimd)]

#[macro_use]
extern crate bencher;

#[macro_use]
extern crate nom;

use bencher::{black_box, Bencher};

use nom::IResult;

#[macro_use]
mod combinators;

use combinators::{is_header_value_token, is_token};

#[derive(Debug)]
struct Request<'a> {
    method: &'a [u8],
    uri: &'a [u8],
    version: u8,
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct Header<'a> {
    name: &'a [u8],
    value: &'a [u8],
}

fn is_url_token(c: u8) -> bool {
    c > 0x20 && c < 0x7F
}

#[inline]
fn is_header_name_token(b: u8) -> bool {
    is_token(b)
}

fn not_line_ending(c: u8) -> bool {
    c != b'\r' && c != b'\n'
}

fn is_space(c: u8) -> bool {
    c == b' '
}

fn is_not_space(c: u8) -> bool {
    c != b' '
}
fn is_horizontal_space(c: u8) -> bool {
    c == b' ' || c == b'\t'
}

fn is_version(c: u8) -> bool {
    c >= b'0' && c <= b'9' || c == b'.'
}

named!(line_ending, alt!(tag!("\r\n") | tag!("\n")));

#[cfg(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2"))]
fn request_line<'a, 'r>(input: &'a [u8], req: &'r mut Request<'a>) -> IResult<&'a [u8], ()> {
    let range = b"\0 \x7F\x7F";
    do_parse!(
        input,
        method: take_while1_unrolled!(is_token) >> char!(' ') >> uri: take_while1_simd!(is_url_token, range)
            >> char!(' ') >> version: http_version >> line_ending >> ({
            req.method = method;
            req.uri = uri;
            req.version = version;
        })
    )
}

#[cfg(not(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2")))]
fn request_line<'a, 'r>(input: &'a [u8], req: &'r mut Request<'a>) -> IResult<&'a [u8], ()> {
    do_parse!(
        input,
        method: take_while1_unrolled!(is_token) >> char!(' ') >> uri: take_while1_unrolled!(is_url_token)
            >> char!(' ') >> version: http_version >> line_ending >> ({
            req.method = method;
            req.uri = uri;
            req.version = version;
        })
    )
}

named!(
    http_version<u8>,
    preceded!(
        tag!("HTTP/1."),
        map!(one_of!("01"), |n| if n == '0' { 0 } else { 1 })
    )
);

//const header_value_range:&[u8] = &[0, 010, 012, 037, 177, 177];
const header_value_range: &[u8] = b"\0\x08\x0A\x1F\x7F\x7F";

#[cfg(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2"))]
named!(
    header_value,
    delimited!(
        take_while1_unrolled!(is_horizontal_space),
        take_while1_simd!(is_header_value_token, header_value_range),
        line_ending
    )
);

#[cfg(not(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2")))]
named!(
    header_value,
    delimited!(
        take_while1_unrolled!(is_horizontal_space),
        take_while1_unrolled!(is_header_value_token),
        line_ending
    )
);

fn header<'a, 'h>(input: &'a [u8], header: &'h mut Header<'a>) -> IResult<&'a [u8], ()> {
    let (input, name) = try_parse!(input, take_while1_unrolled!(is_header_name_token));
    header.name = name;
    let (input, _) = try_parse!(input, char!(':'));
    let (input, value) = try_parse!(input, header_value);
    header.value = value;
    Ok((input, ()))
}

fn headers_iter<'a, 'h>(input: &'a [u8], headers: &'h mut [Header<'a>]) -> IResult<&'a [u8], ()> {
    let mut iter = headers.iter_mut();
    let mut input = input;

    loop {
        let h = match iter.next() {
            Some(header) => header,
            None => break,
        };

        match header(input, h) {
            Ok((i, _)) => input = i,
            Err(nom::Err::Error(_)) => break,
            e => return e,
        }
    }

    Ok((input, ()))
}

fn request<'a, 'r, 'h>(
    input: &'a [u8],
    req: &'r mut Request<'a>,
    headers: &'h mut [Header<'a>],
) -> IResult<&'a [u8], ()> {
    terminated!(
        input,
        apply!(request_line, req),
        pair!(apply!(headers_iter, headers), line_ending)
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

#[test]
fn httparse_test() {
    use std::str;
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

    let mut req = Request {
        method: &[],
        uri: &[],
        version: 0,
    };
    let mut headers = [Header {
        name: &[],
        value: &[],
    }; 16];
    let res = request(data, &mut req, &mut headers);
    println!("res:{:?}\nreq:\n{:?}\nheaders:\n{:?}", res, req, headers);
    if let Err(nom::Err::Error(nom::Context::Code(ref i, ref e))) = res {
        println!(
            "got error {:?} at input:\n{}",
            e,
            str::from_utf8(i).unwrap()
        );
    }
    res.unwrap();
}

fn parse(b: &mut Bencher, buffer: &[u8]) {
    let mut req = Request {
        method: &[],
        uri: &[],
        version: 0,
    };
    let mut headers = [Header {
        name: &[],
        value: &[],
    }; 16];
    b.iter(|| {
        let mut buf = black_box(buffer);
        let mut v = Vec::new();

        while !buf.is_empty() {
            match request(buf, &mut req, &mut headers) {
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

benchmark_group!(
    http,
    one_test,
    small_test,
    bigger_test,
    httparse_example_test
);
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
