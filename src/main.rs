#[macro_use]
extern crate nom;

extern crate test;

use nom::{HexDisplay,IResult,FlatMap,FlatMapOpt,Functor,Producer,ProducerState,FileProducer,be_u16,be_u32,be_u64,be_f32,be_f64};
use nom::{Consumer,ConsumerState};
use nom::IResult::*;

use std::str;
use std::io::SeekFrom;

fn mp4_box(input:&[u8]) -> IResult<&[u8], &[u8]> {
  match be_u32(input) {
    Done(i, offset) => {
      let sz: usize = offset as usize;
      //println!("size: {}", sz);
      if i.len() >= sz - 4 {
        return Done(&i[(sz-4)..], &i[0..(sz-4)])
      } else {
        return Incomplete(1234)
      }
    }
    Error(e)      => Error(e),
    Incomplete(e) => Incomplete(e)
  }
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

tag!(ftyp    "ftyp".as_bytes());

fn brand_name(input:&[u8]) -> IResult<&[u8],&str> {
  take!(major_brand_bytes 4);
  major_brand_bytes(input).map_res(str::from_utf8)
}
take!(major_brand_version 4);
many0!(compatible_brands<&[u8], &str> brand_name);

fn filetype_parser<'a>(input: &'a[u8]) -> IResult<&'a [u8], FileType<'a> > {
  //println!("ftyp:\n{}", input.to_hex(8));
  chaining_parser!(input,
    m: brand_name          ~
    v: major_brand_version ~
    c: compatible_brands   ,
    ||{FileType{major_brand: m, major_brand_version:v, compatible_brands: c}})
}

o!(filetype <&[u8], FileType>  ftyp ~ [ filetype_parser ]);

fn filetype_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  match filetype(input) {
    Error(a)      => Error(a),
    Incomplete(a) => Incomplete(a),
    Done(i, o)    => {
      Done(i, MP4Box::Ftyp(o))
    }
  }
}

tag!(moov_tag "moov".as_bytes());
fn moov_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  moov_tag(input).map(|_| MP4Box::Moov)
}

tag!(mdat    "mdat".as_bytes());
fn mdat_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  mdat(input).map(|_| MP4Box::Mdat)
}
tag!(free    "free".as_bytes());
fn free_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  free(input).map(|_| MP4Box::Free)
}

tag!(skip    "skip".as_bytes());
fn skip_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  skip(input).map(|_| MP4Box::Skip)
}

tag!(wide    "wide".as_bytes());
fn wide_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  wide(input).map(|_| MP4Box::Wide)
}

fn unknown_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  Done(input, MP4Box::Unknown)
}

alt!(box_parser_internal<&[u8], MP4Box>, filetype_box | moov_box | mdat_box | free_box | skip_box | wide_box | unknown_box);
fn box_parser(input:&[u8]) -> IResult<&[u8], MP4Box> {
  mp4_box(input).flat_map(box_parser_internal)
}

fn data_interpreter(bytes:&[u8]) -> IResult<&[u8], ()> {
  //println!("bytes:\n{}", bytes.to_hex(8));
  //println!("bytes length: {}", bytes.len());
  match box_parser(bytes) {
    Done(i, o) => {
      /*match o {
        MP4Box::Ftyp(f) => println!("-> FTYP: {:?}", f),
        MP4Box::Moov(m) => println!("-> MOOV: {:?}", m),
        MP4Box::Mdat    => println!("-> MDAT"),
        MP4Box::Free    => println!("-> FREE"),
        MP4Box::Skip    => println!("-> SKIP"),
        MP4Box::Wide    => println!("-> WIDE"),
        MP4Box::Unknown => println!("-> UNKNOWN")
      }*/
      //println!("remaining:\n{}", i.to_hex(8));
      //println!("got o");
      Done(i,())
    },
    Error(a) => {
      println!("mp4 parsing error: {:?}", a);
      assert!(false);
      Error(a)
    },
    Incomplete(a) => {
      println!("mp4 incomplete: {:?}", a);
      Incomplete(a)
    }
  }
}

many0!(full_data_interpreter<&[u8],()> data_interpreter);

use test::Bencher;
#[bench]
fn small_test(b: &mut Bencher) {
  let data = include_bytes!("../small.mp4");
  b.iter(||{
    full_data_interpreter(data)
  });
}

#[bench]
fn bigbuckbunny_test(b: &mut Bencher) {
  let data = include_bytes!("../bigbuckbunny.mp4");
  b.iter(||{
    full_data_interpreter(data)
  });
}

fn main() {
  println!("Hello, world!");
  let data = include_bytes!("../small.mp4");
  //println!("data:\n{}", data.to_hex(8));
  full_data_interpreter(data);
}
