use std::{error, fmt};

//use crate::parent::{Frag, Parent};

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidChar(char),
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidChar(char) => write!(f, "invalid char: {:?}", char),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Loc(pub usize, pub usize, pub usize);

impl Loc {
    fn shift(self, c: usize) -> Self {
        Loc(self.0 + c, self.1, self.2 + c)
    }

    fn shift_line(self, c: usize) -> Self {
        Loc(self.0 + c, self.1 + 1, 0)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[test]
fn test_loc_shift() {
    assert_eq!(Loc(0, 0, 0).shift(1), Loc(1, 0, 1));
}

#[test]
fn test_loc_shift_line() {
    assert_eq!(Loc(0, 0, 0).shift_line(1), Loc(1, 1, 0));
}

#[derive(Clone, Copy, PartialEq)]
pub struct Spanning<T, U>(pub T, pub U, pub U, pub Option<u8>)
where
    U: Clone;

impl<T, V> Spanning<T, V>
where
    V: Clone,
{
    pub fn map<U>(self, f: fn(T) -> U) -> Spanning<U, V> {
        Spanning(f(self.0), self.1, self.2, self.3)
    }
}

impl<T, U> fmt::Debug for Spanning<T, U>
where
    T: fmt::Debug + Clone,
    U: fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Spanning({:?}, {:?}, {:?}, {:?})",
            self.0, self.1, self.2, self.3,
        )
    }
}

impl<T, U> fmt::Display for Spanning<T, U>
where
    T: fmt::Display + Clone,
    U: Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct Tokenizer<'a> {
    offset: Loc,
    text: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn trim_start(&mut self) {
        loop {
            match self.text.chars().next() {
                Some(' ' | '\t') => self.offset = self.offset.shift(1),
                Some('\n') => self.offset = self.offset.shift_line(1),
                Some('\r') => {}
                Some(';') => {
                    let last = self.text.chars().take_while(|&c| c != '\n').count();
                    if last == self.text.len() - 1 {
                        self.text = &self.text[last..];
                        self.offset = self.offset.shift_line(last + 1);
                    } else {
                        self.text = &self.text[last..];
                        self.offset = self.offset.shift(last);
                    }
                    continue;
                }
                _ => break,
            }
            self.text = &self.text[1..];
        }
    }

    fn take_byte(&mut self) -> Option<Result<Spanning<u8, Loc>, Error>> {
        self.trim_start();

        let (byte, text) = match self.text.chars().next() {
            Some(char) if char.is_digit(16) => match self.text.chars().skip(1).next() {
                Some(char) if char.is_digit(16) => (
                    Spanning(
                        u8::from_str_radix(&self.text[..2], 16).unwrap(),
                        self.offset,
                        self.offset.shift(2),
                        None,
                    ),
                    &self.text[2..],
                ),
                None => (
                    Spanning(
                        u8::from_str_radix(&self.text[..1], 16).unwrap(),
                        self.offset,
                        self.offset.shift(1),
                        None,
                    ),
                    &self.text[1..],
                ),
                Some(char) if char.is_whitespace() => (
                    Spanning(
                        u8::from_str_radix(&self.text[..1], 16).unwrap(),
                        self.offset,
                        self.offset.shift(1),
                        None,
                    ),
                    &self.text[1..],
                ),
                Some(char) => return Some(Err(Error::InvalidChar(char))),
            },
            Some(char) => return Some(Err(Error::InvalidChar(char))),
            None => {
                return None;
            }
        };
        self.offset = self.offset.shift(self.text.len() - text.len());
        self.text = text;
        Some(Ok(byte))
    }
}

#[test]
fn test_tokenizer_trim_start() {
    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(0, 0, 0));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "  ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(2, 0, 2));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "\t\r\n",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(2, 1, 0));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: " ;  ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 0, 4));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: " ; hello world ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(15, 0, 15));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "1312 ; baca ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(0, 0, 0));
    assert_eq!(t.text, "1312 ; baca ");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "\t\t  4200\n\t\t  1337",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 0, 4));
    assert_eq!(t.text, "4200\n\t\t  1337");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 0, 4));
    assert_eq!(t.text, "f4c3 ; book\n    c00b ; ecaf\n");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(31, 2, 0));
    assert_eq!(t.text, "ffff");
}

#[test]
fn test_tokenizer_take_byte() {
    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(0, 0, 0));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(2, 0, 2));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "\t\r\n",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(2, 1, 0));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: " ;  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(4, 0, 4));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: " ; hello world ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(15, 0, 15));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "1312 ; baca ",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0x13, Loc(0, 0, 0), Loc(2, 0, 2), None))),
    );
    assert_eq!(t.offset, Loc(2, 0, 2));
    assert_eq!(t.text, "12 ; baca ");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "\t\t  4200\n\t\t  1337",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0x42, Loc(4, 0, 4), Loc(6, 0, 6), None))),
    );
    assert_eq!(t.offset, Loc(6, 0, 6));
    assert_eq!(t.text, "00\n\t\t  1337");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0xf4, Loc(4, 0, 4), Loc(6, 0, 6), None))),
    );
    assert_eq!(t.offset, Loc(6, 0, 6));
    assert_eq!(t.text, "c3 ; book\n    c00b ; ecaf\n");

    let mut t = Tokenizer {
        offset: Loc(0, 0, 0),
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0xff, Loc(31, 2, 0), Loc(33, 2, 2), None))),
    );
    assert_eq!(t.offset, Loc(33, 2, 2));
    assert_eq!(t.text, "ff");
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Spanning<u8, Loc>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.take_byte()
    }
}

pub fn tokenize(text: &str) -> Result<Vec<Spanning<u8, Loc>>, Error> {
    Ok(Tokenizer {
        offset: Loc(0, 0, 0),
        text,
    }
    .collect::<Result<Vec<_>, _>>()?)
}

#[test]
fn test_tokenize() {
    assert_eq!(
        tokenize("0").unwrap(),
        vec![Spanning(0, Loc(0, 0, 0), Loc(1, 0, 1), None)],
    );
    assert_eq!(
        tokenize("00").unwrap(),
        vec![Spanning(0, Loc(0, 0, 0), Loc(2, 0, 2), None)],
    );
    assert_eq!(
        tokenize("  00\r\n").unwrap(),
        vec![Spanning(0, Loc(2, 0, 2), Loc(4, 0, 4), None)],
    );
    assert_eq!(
        tokenize("0 12 345 6789").unwrap(),
        vec![
            Spanning(0, Loc(0, 0, 0), Loc(1, 0, 1), None),
            Spanning(0x12, Loc(2, 0, 2), Loc(4, 0, 4), None),
            Spanning(0x34, Loc(5, 0, 5), Loc(7, 0, 7), None),
            Spanning(5, Loc(7, 0, 7), Loc(8, 0, 8), None),
            Spanning(0x67, Loc(9, 0, 9), Loc(11, 0, 11), None),
            Spanning(0x89, Loc(11, 0, 11), Loc(13, 0, 13), None),
        ],
    );
    assert_eq!(
        tokenize("1312 dead beef").unwrap(),
        vec![
            Spanning(0x13, Loc(0, 0, 0), Loc(2, 0, 2), None),
            Spanning(0x12, Loc(2, 0, 2), Loc(4, 0, 4), None),
            Spanning(0xde, Loc(5, 0, 5), Loc(7, 0, 7), None),
            Spanning(0xad, Loc(7, 0, 7), Loc(9, 0, 9), None),
            Spanning(0xbe, Loc(10, 0, 10), Loc(12, 0, 12), None),
            Spanning(0xef, Loc(12, 0, 12), Loc(14, 0, 14), None),
        ],
    );
    assert_eq!(tokenize("; blank comment thing").unwrap(), vec![]);
    assert_eq!(
        tokenize(
            r#"; setuid(0)
        31 c0                   ; xor    %eax,%eax
        50                      ; push   %eax
        50                      ; push   %eax
        b0 17                   ; mov    $0x17,%al
        cd 80                   ; int    $0x80

; execve("/bin/sh", ["/bin/sh"], NULL)
        31 c0                   ; xor    %eax,%eax
        50                      ; push   %eax
        68 2f 2f 73 68          ; push   $0x68732f2f
        68 2f 62 69 6e          ; push   $0x6e69622f
        89 e3                   ; mov    %esp,%ebx
        50                      ; push   %eax
        54                      ; push   %esp
        53                      ; push   %ebx
        50                      ; push   %eax
        b0 3b                   ; mov    $0x3b,%al
        cd 80                   ; int    $0x80"#
        )
        .unwrap(),
        vec![
            Spanning(0x31, Loc(20, 1, 8), Loc(22, 1, 10), None),
            Spanning(0xc0, Loc(23, 1, 11), Loc(25, 1, 13), None),
            Spanning(0x50, Loc(71, 2, 8), Loc(73, 2, 10), None),
            Spanning(0x50, Loc(117, 3, 8), Loc(119, 3, 10), None),
            Spanning(0xb0, Loc(163, 4, 8), Loc(165, 4, 10), None),
            Spanning(0x17, Loc(166, 4, 11), Loc(168, 4, 13), None),
            Spanning(0xcd, Loc(214, 5, 8), Loc(216, 5, 10), None),
            Spanning(0x80, Loc(217, 5, 11), Loc(219, 5, 13), None),
            Spanning(0x31, Loc(301, 8, 8), Loc(303, 8, 10), None),
            Spanning(0xc0, Loc(304, 8, 11), Loc(306, 8, 13), None),
            Spanning(0x50, Loc(352, 9, 8), Loc(354, 9, 10), None),
            Spanning(0x68, Loc(398, 10, 8), Loc(400, 10, 10), None),
            Spanning(0x2f, Loc(401, 10, 11), Loc(403, 10, 13), None),
            Spanning(0x2f, Loc(404, 10, 14), Loc(406, 10, 16), None),
            Spanning(0x73, Loc(407, 10, 17), Loc(409, 10, 19), None),
            Spanning(0x68, Loc(410, 10, 20), Loc(412, 10, 22), None),
            Spanning(0x68, Loc(451, 11, 8), Loc(453, 11, 10), None),
            Spanning(0x2f, Loc(454, 11, 11), Loc(456, 11, 13), None),
            Spanning(0x62, Loc(457, 11, 14), Loc(459, 11, 16), None),
            Spanning(0x69, Loc(460, 11, 17), Loc(462, 11, 19), None),
            Spanning(0x6e, Loc(463, 11, 20), Loc(465, 11, 22), None),
            Spanning(0x89, Loc(504, 12, 8), Loc(506, 12, 10), None),
            Spanning(0xe3, Loc(507, 12, 11), Loc(509, 12, 13), None),
            Spanning(0x50, Loc(555, 13, 8), Loc(557, 13, 10), None),
            Spanning(0x54, Loc(601, 14, 8), Loc(603, 14, 10), None),
            Spanning(0x53, Loc(647, 15, 8), Loc(649, 15, 10), None),
            Spanning(0x50, Loc(693, 16, 8), Loc(695, 16, 10), None),
            Spanning(0xb0, Loc(739, 17, 8), Loc(741, 17, 10), None),
            Spanning(0x3b, Loc(742, 17, 11), Loc(744, 17, 13), None),
            Spanning(0xcd, Loc(790, 18, 8), Loc(792, 18, 10), None),
            Spanning(0x80, Loc(793, 18, 11), Loc(795, 18, 13), None),
        ],
    );
}
