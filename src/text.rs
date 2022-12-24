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
        Loc(self.0 + c, self.1 + 1, 1)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[test]
fn test_loc_shift() {
    assert_eq!(Loc(0, 1, 1).shift(1), Loc(1, 1, 2));
}

#[test]
fn test_loc_shift_line() {
    assert_eq!(Loc(0, 1, 1).shift_line(1), Loc(1, 2, 1));
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
        offset: Loc(0, 1, 1),
        text: "",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(0, 1, 1));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "  ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(2, 1, 3));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "\t\r\n",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(2, 2, 1));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: " ;  ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 1, 5));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: " ; hello world ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(15, 1, 16));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "1312 ; baca ",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(0, 1, 1));
    assert_eq!(t.text, "1312 ; baca ");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "\t\t  4200\n\t\t  1337",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 1, 5));
    assert_eq!(t.text, "4200\n\t\t  1337");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(4, 1, 5));
    assert_eq!(t.text, "f4c3 ; book\n    c00b ; ecaf\n");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    t.trim_start();
    assert_eq!(t.offset, Loc(31, 3, 1));
    assert_eq!(t.text, "ffff");
}

#[test]
fn test_tokenizer_take_byte() {
    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(0, 1, 1));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(2, 1, 3));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "\t\r\n",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(2, 2, 1));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: " ;  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(4, 1, 5));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: " ; hello world ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(t.offset, Loc(15, 1, 16));
    assert_eq!(t.text, "");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "1312 ; baca ",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0x13, Loc(0, 1, 1), Loc(2, 1, 3), None))),
    );
    assert_eq!(t.offset, Loc(2, 1, 3));
    assert_eq!(t.text, "12 ; baca ");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "\t\t  4200\n\t\t  1337",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0x42, Loc(4, 1, 5), Loc(6, 1, 7), None))),
    );
    assert_eq!(t.offset, Loc(6, 1, 7));
    assert_eq!(t.text, "00\n\t\t  1337");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0xf4, Loc(4, 1, 5), Loc(6, 1, 7), None))),
    );
    assert_eq!(t.offset, Loc(6, 1, 7));
    assert_eq!(t.text, "c3 ; book\n    c00b ; ecaf\n");

    let mut t = Tokenizer {
        offset: Loc(0, 1, 1),
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    assert_eq!(
        t.take_byte(),
        Some(Ok(Spanning(0xff, Loc(31, 3, 1), Loc(33, 3, 3), None))),
    );
    assert_eq!(t.offset, Loc(33, 3, 3));
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
        offset: Loc(0, 1, 1),
        text,
    }
    .collect::<Result<Vec<_>, _>>()?)
}

#[test]
fn test_tokenize() {
    assert_eq!(
        tokenize("0").unwrap(),
        vec![Spanning(0, Loc(0, 1, 1), Loc(1, 1, 2), None)],
    );
    assert_eq!(
        tokenize("00").unwrap(),
        vec![Spanning(0, Loc(0, 1, 1), Loc(2, 1, 3), None)],
    );
    assert_eq!(
        tokenize("  00\r\n").unwrap(),
        vec![Spanning(0, Loc(2, 1, 3), Loc(4, 1, 5), None)],
    );
    assert_eq!(
        tokenize("0 12 345 6789").unwrap(),
        vec![
            Spanning(0, Loc(0, 1, 1), Loc(1, 1, 2), None),
            Spanning(0x12, Loc(2, 1, 3), Loc(4, 1, 5), None),
            Spanning(0x34, Loc(5, 1, 6), Loc(7, 1, 8), None),
            Spanning(5, Loc(7, 1, 8), Loc(8, 1, 9), None),
            Spanning(0x67, Loc(9, 1, 10), Loc(11, 1, 12), None),
            Spanning(0x89, Loc(11, 1, 12), Loc(13, 1, 14), None),
        ],
    );
    assert_eq!(
        tokenize("1312 dead beef").unwrap(),
        vec![
            Spanning(0x13, Loc(0, 1, 1), Loc(2, 1, 3), None),
            Spanning(0x12, Loc(2, 1, 3), Loc(4, 1, 5), None),
            Spanning(0xde, Loc(5, 1, 6), Loc(7, 1, 8), None),
            Spanning(0xad, Loc(7, 1, 8), Loc(9, 1, 10), None),
            Spanning(0xbe, Loc(10, 1, 11), Loc(12, 1, 13), None),
            Spanning(0xef, Loc(12, 1, 13), Loc(14, 1, 15), None),
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
            Spanning(0x31, Loc(20, 2, 9), Loc(22, 2, 11), None),
            Spanning(0xc0, Loc(23, 2, 12), Loc(25, 2, 14), None),
            Spanning(0x50, Loc(71, 3, 9), Loc(73, 3, 11), None),
            Spanning(0x50, Loc(117, 4, 9), Loc(119, 4, 11), None),
            Spanning(0xb0, Loc(163, 5, 9), Loc(165, 5, 11), None),
            Spanning(0x17, Loc(166, 5, 12), Loc(168, 5, 14), None),
            Spanning(0xcd, Loc(214, 6, 9), Loc(216, 6, 11), None),
            Spanning(0x80, Loc(217, 6, 12), Loc(219, 6, 14), None),
            Spanning(0x31, Loc(301, 9, 9), Loc(303, 9, 11), None),
            Spanning(0xc0, Loc(304, 9, 12), Loc(306, 9, 14), None),
            Spanning(0x50, Loc(352, 10, 9), Loc(354, 10, 11), None),
            Spanning(0x68, Loc(398, 11, 9), Loc(400, 11, 11), None),
            Spanning(0x2f, Loc(401, 11, 12), Loc(403, 11, 14), None),
            Spanning(0x2f, Loc(404, 11, 15), Loc(406, 11, 17), None),
            Spanning(0x73, Loc(407, 11, 18), Loc(409, 11, 20), None),
            Spanning(0x68, Loc(410, 11, 21), Loc(412, 11, 23), None),
            Spanning(0x68, Loc(451, 12, 9), Loc(453, 12, 11), None),
            Spanning(0x2f, Loc(454, 12, 12), Loc(456, 12, 14), None),
            Spanning(0x62, Loc(457, 12, 15), Loc(459, 12, 17), None),
            Spanning(0x69, Loc(460, 12, 18), Loc(462, 12, 20), None),
            Spanning(0x6e, Loc(463, 12, 21), Loc(465, 12, 23), None),
            Spanning(0x89, Loc(504, 13, 9), Loc(506, 13, 11), None),
            Spanning(0xe3, Loc(507, 13, 12), Loc(509, 13, 14), None),
            Spanning(0x50, Loc(555, 14, 9), Loc(557, 14, 11), None),
            Spanning(0x54, Loc(601, 15, 9), Loc(603, 15, 11), None),
            Spanning(0x53, Loc(647, 16, 9), Loc(649, 16, 11), None),
            Spanning(0x50, Loc(693, 17, 9), Loc(695, 17, 11), None),
            Spanning(0xb0, Loc(739, 18, 9), Loc(741, 18, 11), None),
            Spanning(0x3b, Loc(742, 18, 12), Loc(744, 18, 14), None),
            Spanning(0xcd, Loc(790, 19, 9), Loc(792, 19, 11), None),
            Spanning(0x80, Loc(793, 19, 12), Loc(795, 19, 14), None),
        ],
    );
}
