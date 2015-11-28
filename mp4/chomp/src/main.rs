#![feature(test)]

#[macro_use]
extern crate chomp;

extern crate test;

use chomp::*;
use chomp::buffer::{IntoStream, Stream};

use std::str::from_utf8;

fn be_u32(i: Input<u8>) -> U8Result<u32> {
    take(i, 4).bind(|i, b|
                i.ret(((b[0] as u32) << 24) + ((b[1] as u32) << 16) + ((b[2] as u32) << 8) + b[3] as u32))
}

fn mp4_box(i: Input<u8>) -> U8Result<&[u8]> {
    be_u32(i).bind(|i, n| take(i, n as usize - 4))
}

#[derive(PartialEq,Eq,Debug)]
struct FileType<'a> {
  major_brand:         &'a str,
  major_brand_version: &'a [u8],
  compatible_brands:   Vec<&'a str>
}

#[derive(Debug)]
enum MP4Box<'a> {
  Ftyp(FileType<'a>),
  Moov,
  Mdat,
  Free,
  Skip,
  Wide,
  Unknown
}

fn brand_name(i: Input<u8>) -> U8Result<&str> {
    take(i, 4).bind(|i, s|
                    match from_utf8(s) {
                        Ok(s)  => i.ret(s),
                        Err(_) => i.err(Error::new())
                    })
}

fn ftyp(i: Input<u8>) -> U8Result<&[u8]> {
    string(i, b"ftyp")
}

fn filetype_box(i: Input<u8>) -> U8Result<MP4Box> {
    parse!{i;
        ftyp();
        let m = brand_name();
        let v = take(4);
        let c = many(brand_name);
        ret MP4Box::Ftyp(FileType{major_brand: m, major_brand_version:v, compatible_brands: c})}
}

fn box_parser_internal(i: Input<u8>) -> U8Result<MP4Box> {
    // TODO: This is ugly (and slow in verbose_error):
    or(i, |i| filetype_box(i), |i|
    or(i, parser!{string(b"moov"); ret MP4Box::Moov}, |i|
    or(i, parser!{string(b"mdat"); ret MP4Box::Mdat}, |i|
    or(i, parser!{string(b"free"); ret MP4Box::Free}, |i|
    or(i, parser!{string(b"skip"); ret MP4Box::Skip}, |i|
    or(i, parser!{string(b"wide"); ret MP4Box::Wide},
           |i| i.ret(MP4Box::Unknown)))))))
}

fn box_parser(i: Input<u8>) -> U8Result<MP4Box> {
    mp4_box(i).bind(|i, b| i.from_result(b.into_stream().parse(box_parser_internal).map_err(|_| Error::new())))
}

fn full_data_interpreter(i: Input<u8>) -> U8Result<Vec<MP4Box>> {
    many(i, box_parser)
}

use test::Bencher;

#[bench]
fn small_test(b: &mut Bencher) {
  let data = include_bytes!("../../small.mp4");
  b.iter(||{
    data.into_stream().parse(full_data_interpreter)
  });
}

#[bench]
fn bigbuckbunny_test(b: &mut Bencher) {
  let data = include_bytes!("../../bigbuckbunny.mp4");
  b.iter(||{
    data.into_stream().parse(full_data_interpreter)
  });
}

fn main() {
  println!("Hello, world!");
  let data = include_bytes!("../../small.mp4");
  //println!("data:\n{}", data.to_hex(8));
  loop {
    //let res = full_data_interpreter(data);
    //println!("res: {:?}", res);
    data.into_stream().parse(box_parser).unwrap();
  }
}
