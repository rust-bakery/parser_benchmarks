#[macro_use]
extern crate bencher;

extern crate pico_sys as pico;

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
  use std::mem;

  #[repr(C)]
  #[derive(Clone, Copy)]
  struct Header<'a>(&'a [u8], &'a [u8]);

  #[repr(C)]
  struct Headers<'a>(&'a mut [Header<'a>]);

  b.bytes = buffer.len() as u64;
  b.iter(|| {
    let mut buf = black_box(buffer);
    let mut index = 0;
    let len = buf.len();
    while  index < len {
      let slice = &buf[index..];

      let method = [0i8; 16];
      let path = [0i8; 100];
      let mut minor_version = 0;
      let mut h = [Header(&[], &[]); 16];
      let mut h_len = h.len();
      let headers = Headers(&mut h);
      let prev_buf_len = 0;

      let ret = unsafe {
        pico::ffi::phr_parse_request(
          slice.as_ptr() as *const _,
          slice.len(),
          &mut method.as_ptr(),
          &mut 16,
          &mut path.as_ptr(),
          &mut 100,
          &mut minor_version,
          mem::transmute::<*mut Header, *mut pico::ffi::phr_header>(headers.0.as_mut_ptr()),
          &mut h_len as *mut usize as *mut _,
          prev_buf_len
        )
      };

      if ret < 0 {
        println!("error: ret = {}", ret);
        break;
      }

      index += ret as usize;
    }
  });
}

benchmark_group!(http, one_test, small_test, bigger_test, httparse_example_test);
benchmark_main!(http);
