#[macro_use]
extern crate bencher;

extern crate httparse;

use bencher::{black_box, Bencher};

//FIXME: didn't find a way to parse headers in a loop yet
/*
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
*/

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

fn parse(b: &mut Bencher, buffer: &[u8]) {
  b.iter(|| {
      let mut buf = black_box(buffer);
      let mut index = 0;
      let len = buf.len();
      while  index < len {
        let slice = &buf[index..];

        let mut headers = [httparse::Header{ name: "", value: &[] }; 16];
        let mut req = httparse::Request::new(&mut headers);

        if let httparse::Status::Complete(sz) = req.parse(buf).unwrap() {
          index += sz;
        } else {
          panic!("parse error");
        }
      }
  });

  /*
  b.iter(|| {
    let mut buf = black_box(buffer);
    let mut v = Vec::new();

    while !buf.is_empty() {
      match req.parse(buf) {
        Ok(httparse::Status::Complete(len)) => {
          v.push(len);

          buf = &buf[len..];
        },
        Ok(e) => panic!("got partial: {:?}", e),
        Err(err) => panic!("got err: {:?}", err),
      }
    }

    v
  });
  */
}

benchmark_group!(http, one_test, small_test, bigger_test, httparse_example_test);
benchmark_main!(http);
