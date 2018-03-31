use nom::IResult;
use nom::HexDisplay;

#[macro_export]
macro_rules! take_while1_unrolled (
  ($input:expr, $predicate: expr) => (
    {
      use nom::Err;
      use nom::Context;
      use nom::Needed;
      use nom::ErrorKind;

      let input = $input;

      let mut i = 0usize;
      let len = input.len();
      let mut found = false;

      loop {
        if len - i < 8 {
          break;
        }

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;

        if !$predicate(unsafe { *input.get_unchecked(i) }) {
          found = true;
          break;
        }
        i = i+1;
      }

      if !found {
        loop {
          if !$predicate(unsafe { *input.get_unchecked(i) }) {
            break;
          }
          i = i+1;
          if i == len {
            break;
          }
        }
      }

      if i == 0 {
        Err(Err::Error(Context::Code(input, ErrorKind::TakeWhile1)))
      } else if i == len {
        Err(Err::Incomplete(Needed::Unknown))
      } else {
        let (prefix, suffix) = input.split_at(i);
        Ok((suffix, prefix))
      }
    }
  );
);

#[macro_export]
macro_rules! take_while1_simd (
  ($input:expr, $predicate:expr, $ranges:expr) => ({
      #[cfg(feature = "simd")]
      use stdsimd::vendor::*;

      use nom::Err;
      use nom::Context;
      use nom::Needed;
      use nom::ErrorKind;

      let input = $input;

      let mut start = input.as_ptr() as usize;
      let mut i = input.as_ptr() as usize;
      let mut left = input.len();
      let mut found = false;

      if left >= 16 {

        let ranges16 = unsafe { _mm_loadu_si128($ranges.as_ptr()  as *const _) };
        let ranges_len = $ranges.len() as i32;
        loop {
          let sl = unsafe { _mm_loadu_si128(i  as *const _) };

          let idx = unsafe {
            _mm_cmpestri(
              ranges16, ranges_len,
              sl, 16,
              _SIDD_LEAST_SIGNIFICANT | _SIDD_CMP_RANGES | _SIDD_UBYTE_OPS)
          };

          if idx != 16 {
            i += idx as usize;
            found = true;
            break;
          }

          i += 16;
          left -= 16;

          if left < 16 {
            break;
          }
        }
      }

      let mut i = i - start;
      if !found {
        loop {
          if !$predicate(unsafe { *input.get_unchecked(i) }) {
            break;
          }
          i = i+1;
          if i == input.len() {
            break;
          }
        }
      }

      if i == 0 {
        Err(Err::Error(Context::Code(input, ErrorKind::TakeWhile1)))
      } else if i == input.len() {
        Err(Err::Incomplete(Needed::Unknown))
      } else {
        let (prefix, suffix) = input.split_at(i);
        Ok((suffix, prefix))
      }
  })
);

#[test]
fn simd_test() {
  //let range = &[0, 040, 177, 177];
  //let range = &[0, 32, 127, 127];
  let range = b"\0 \x7F\x7F";
  let input = b"/abcd/efgh/ijkl/pouet/ 1234579";
  //let input = b"/a  bcd/efgh/ijkl/pouet/ 1234579";
  let input = b"/abcd/efgh/ij kl/pouet/ 1234579";
  let res: IResult<&[u8], &[u8]> = take_while1_simd!(input, is_token, range);

  let (i, o) = res.unwrap();
  println!("i = '{}', o = '{}'", std::str::from_utf8(i).unwrap(), std::str::from_utf8(o).unwrap());
  //assert!(false);
}
