use std::{error, fmt};

use crate::Spanning;

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

#[derive(Debug, PartialEq)]
pub struct Tokenizer<'a> {
    offset: usize,
    text: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn trim_start(&mut self) {
        let p = regex::Regex::new("^([ \n\r\t]*(;.*)?)*").unwrap();
        let len = self.text.len();
        self.text = self.text.trim_start_matches(&p);
        self.offset += len - self.text.len();
    }

    fn take_byte(&mut self) -> Option<Result<Spanning<u8>, Error>> {
        self.trim_start();

        let (byte, tail) = match self.text.chars().next() {
            Some(char) if char.is_digit(16) => match self.text.chars().skip(1).next() {
                Some(char) if char.is_digit(16) => (
                    Spanning(
                        u8::from_str_radix(&self.text[..2], 16).unwrap(),
                        self.offset,
                        2,
                        None,
                    ),
                    &self.text[2..],
                ),
                None => (
                    Spanning(
                        u8::from_str_radix(&self.text[..1], 16).unwrap(),
                        self.offset,
                        1,
                        None,
                    ),
                    &self.text[1..],
                ),
                Some(char) if char.is_whitespace() => (
                    Spanning(
                        u8::from_str_radix(&self.text[..1], 16).unwrap(),
                        self.offset,
                        1,
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
        let len = self.text.len();
        self.text = tail;
        self.offset += len - self.text.len();
        Some(Ok(byte))
    }
}

#[test]
fn test_tokenizer_trim_start() {
    let mut t = Tokenizer {
        offset: 0,
        text: "",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 0,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "  ",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 2,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "\t\r\n",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 3,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: " ;  ",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 4,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: " ; hello world ",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 15,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "1312 ; baca ",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 0,
            text: "1312 ; baca ",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "\t\t  4200\n\t\t  1337",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 4,
            text: "4200\n\t\t  1337",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 4,
            text: "f4c3 ; book\n    c00b ; ecaf\n",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    t.trim_start();
    assert_eq!(
        t,
        Tokenizer {
            offset: 31,
            text: "ffff",
        },
    );
}

#[test]
fn test_tokenizer_take_byte() {
    let mut t = Tokenizer {
        offset: 0,
        text: "",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(
        t,
        Tokenizer {
            offset: 0,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(
        t,
        Tokenizer {
            offset: 2,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "\t\r\n",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(
        t,
        Tokenizer {
            offset: 3,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: " ;  ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(
        t,
        Tokenizer {
            offset: 4,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: " ; hello world ",
    };
    assert_eq!(t.take_byte(), None);
    assert_eq!(
        t,
        Tokenizer {
            offset: 15,
            text: "",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "1312 ; baca ",
    };
    assert_eq!(t.take_byte(), Some(Ok(Spanning(0x13, 0, 2, None))));
    assert_eq!(
        t,
        Tokenizer {
            offset: 2,
            text: "12 ; baca ",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "\t\t  4200\n\t\t  1337",
    };
    assert_eq!(t.take_byte(), Some(Ok(Spanning(0x42, 4, 2, None))));
    assert_eq!(
        t,
        Tokenizer {
            offset: 6,
            text: "00\n\t\t  1337",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "    f4c3 ; book\n    c00b ; ecaf\n",
    };
    assert_eq!(t.take_byte(), Some(Ok(Spanning(0xf4, 4, 2, None))));
    assert_eq!(
        t,
        Tokenizer {
            offset: 6,
            text: "c3 ; book\n    c00b ; ecaf\n",
        },
    );

    let mut t = Tokenizer {
        offset: 0,
        text: "; lorem ipsum\n; dolor sit amet\nffff",
    };
    assert_eq!(t.take_byte(), Some(Ok(Spanning(0xff, 31, 2, None))));
    assert_eq!(
        t,
        Tokenizer {
            offset: 33,
            text: "ff",
        },
    );
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Spanning<u8>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.take_byte()
    }
}

pub fn tokenize(text: &str) -> Result<Vec<Spanning<u8>>, Error> {
    Ok(Tokenizer { offset: 0, text }.collect::<Result<Vec<_>, _>>()?)
}

#[test]
fn test_tokenize() {
    assert_eq!(tokenize("0").unwrap(), vec![Spanning(0, 0, 1, None)]);
    assert_eq!(tokenize("00").unwrap(), vec![Spanning(0, 0, 2, None)]);
    assert_eq!(tokenize("  00\r\n").unwrap(), vec![Spanning(0, 2, 2, None)]);
    assert_eq!(
        tokenize("0 12 345 6789").unwrap(),
        vec![
            Spanning(0, 0, 1, None),
            Spanning(0x12, 2, 2, None),
            Spanning(0x34, 5, 2, None),
            Spanning(5, 7, 1, None),
            Spanning(0x67, 9, 2, None),
            Spanning(0x89, 11, 2, None),
        ],
    );
    assert_eq!(
        tokenize("1312 dead beef").unwrap(),
        vec![
            Spanning(0x13, 0, 2, None),
            Spanning(0x12, 2, 2, None),
            Spanning(0xde, 5, 2, None),
            Spanning(0xad, 7, 2, None),
            Spanning(0xbe, 10, 2, None),
            Spanning(0xef, 12, 2, None),
        ],
    );
    assert_eq!(
        tokenize("1312 dead beef").unwrap(),
        vec![
            Spanning(0x13, 0, 2, None),
            Spanning(0x12, 2, 2, None),
            Spanning(0xde, 5, 2, None),
            Spanning(0xad, 7, 2, None),
            Spanning(0xbe, 10, 2, None),
            Spanning(0xef, 12, 2, None),
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
            Spanning(0x31, 20, 2, None),
            Spanning(0xc0, 23, 2, None),
            Spanning(0x50, 71, 2, None),
            Spanning(0x50, 117, 2, None),
            Spanning(0xb0, 163, 2, None),
            Spanning(0x17, 166, 2, None),
            Spanning(0xcd, 214, 2, None),
            Spanning(0x80, 217, 2, None),
            Spanning(0x31, 301, 2, None),
            Spanning(0xc0, 304, 2, None),
            Spanning(0x50, 352, 2, None),
            Spanning(0x68, 398, 2, None),
            Spanning(0x2f, 401, 2, None),
            Spanning(0x2f, 404, 2, None),
            Spanning(0x73, 407, 2, None),
            Spanning(0x68, 410, 2, None),
            Spanning(0x68, 451, 2, None),
            Spanning(0x2f, 454, 2, None),
            Spanning(0x62, 457, 2, None),
            Spanning(0x69, 460, 2, None),
            Spanning(0x6e, 463, 2, None),
            Spanning(0x89, 504, 2, None),
            Spanning(0xe3, 507, 2, None),
            Spanning(0x50, 555, 2, None),
            Spanning(0x54, 601, 2, None),
            Spanning(0x53, 647, 2, None),
            Spanning(0x50, 693, 2, None),
            Spanning(0xb0, 739, 2, None),
            Spanning(0x3b, 742, 2, None),
            Spanning(0xcd, 790, 2, None),
            Spanning(0x80, 793, 2, None),
        ],
    );
}
