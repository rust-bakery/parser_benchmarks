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

