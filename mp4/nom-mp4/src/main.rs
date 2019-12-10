#[macro_use]
extern crate bencher;

#[macro_use]
extern crate nom;

use bencher::{Bencher,black_box};
use nom::{IResult, Needed, Err, number::streaming::be_u32};

use std::str::from_utf8;

fn mp4_box(input:&[u8]) -> IResult<&[u8], &[u8]> {
  match be_u32(input) {
    Ok((i, offset)) => {
      let sz: usize = offset as usize;
      //println!("size: {}", sz);
      if i.len() >= sz - 4 {
        return Ok((&i[(sz-4)..], &i[0..(sz-4)]))
      } else {
        return Err(Err::Incomplete(Needed::Size(4 + offset as usize)))
      }
    }
    Err(e) => Err(e),
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

named!(brand_name<&[u8],&str>, map_res!(take!(4), from_utf8));
named!(ftyp, tag!("ftyp"));

fn filetype_box<'a>(input: &'a[u8]) -> IResult<&'a [u8], MP4Box > {
  //println!("ftyp:\n{}", input.to_hex(8));
  do_parse!(input,
       ftyp               >>
    m: brand_name         >>
    v: take!(4)           >>
    c: many0!(brand_name) >>
    (
      MP4Box::Ftyp(FileType{
        major_brand: m,
        major_brand_version: v,
        compatible_brands: c
      })
    )
  )
}

fn unknown_box(input:&[u8]) -> IResult<&[u8], MP4Box> {
  Ok((input, MP4Box::Unknown))
}

named!(box_parser_internal<&[u8], MP4Box>,
  alt!(
    filetype_box |
    tag!("moov") => { |_| MP4Box::Moov } |
    tag!("mdat") => { |_| MP4Box::Mdat } |
    tag!("free") => { |_| MP4Box::Free } |
    tag!("skip") => { |_| MP4Box::Skip } |
    tag!("wide") => { |_| MP4Box::Wide } |
    unknown_box
  )
);

fn box_parser(input:&[u8]) -> IResult<&[u8], MP4Box> {
  //mp4_box(input).flat_map(box_parser_internal)
  flat_map!(input, call!(mp4_box), call!(box_parser_internal))
}

fn data_interpreter(bytes:&[u8]) -> IResult<&[u8], MP4Box> {
  //println!("bytes:\n{}", bytes.to_hex(8));
  //println!("bytes length: {}", bytes.len());
  match box_parser(bytes) {
    Ok((i, o)) => {
      /*match o {
        MP4Box::Ftyp(f) => println!("-> FTYP: {:?}", f),
        MP4Box::Moov    => println!("-> MOOV"),
        MP4Box::Mdat    => println!("-> MDAT"),
        MP4Box::Free    => println!("-> FREE"),
        MP4Box::Skip    => println!("-> SKIP"),
        MP4Box::Wide    => println!("-> WIDE"),
        MP4Box::Unknown => println!("-> UNKNOWN")
      }*/
      //println!("remaining:\n{}", i.to_hex(8));
      //println!("got o");
      Ok((i,o))
    },
    Err(Err::Error(a)) => {
      println!("mp4 parsing error: {:?}", a);
      assert!(false);
      Err(Err::Error(a))
    },
    Err(Err::Failure(a)) => {
      println!("mp4 parsing error: {:?}", a);
      assert!(false);
      Err(Err::Failure(a))
    },
    Err(Err::Incomplete(a)) => {
      //println!("mp4 incomplete: {:?}", a);
      Err(Err::Incomplete(a))
    }
  }
}

named!(full_data_interpreter(&[u8]) -> Vec<MP4Box>, many0!(data_interpreter));

fn small_test(b: &mut Bencher) {
  let data = include_bytes!("../../small.mp4");
  b.iter(||{
    let buf = black_box(data);
    full_data_interpreter(buf).unwrap()
  });
}

fn bigbuckbunny_test(b: &mut Bencher) {
  let data = include_bytes!("../../bigbuckbunny.mp4");
  b.iter(||{
    let buf = black_box(data);
    full_data_interpreter(buf).unwrap()
  });
}

benchmark_group!(mp4, small_test, bigbuckbunny_test);
benchmark_main!(mp4);

/*
fn main() {
  println!("Hello, world!");
  let data = include_bytes!("../../small.mp4");
  //println!("data:\n{}", data.to_hex(8));
  loop {
    //let res = full_data_interpreter(data);
    //println!("res: {:?}", res);
    data_interpreter(data);
  }
}
*/

