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

pub fn tokenize(input: &str) -> Result<Vec<Spanning<u8>>, Error> {
    let mut code = input.trim_start();
    let mut bytes = vec![];
    while !code.is_empty() {
        let byte;
        (byte, code) = match code.chars().next().unwrap() {
            char if char.is_digit(16) => match code.chars().skip(1).next() {
                Some(char) if char.is_digit(16) => (
                    Spanning(
                        u8::from_str_radix(&code[..2], 16).unwrap(),
                        input.len() - code.len(),
                        2,
                        None,
                    ),
                    &code[2..],
                ),
                None => (
                    Spanning(
                        u8::from_str_radix(&code[..1], 16).unwrap(),
                        input.len() - code.len(),
                        1,
                        None,
                    ),
                    &code[1..],
                ),
                Some(char) if char.is_whitespace() => (
                    Spanning(
                        u8::from_str_radix(&code[..1], 16).unwrap(),
                        input.len() - code.len(),
                        1,
                        None,
                    ),
                    &code[1..],
                ),
                Some(char) => return Err(Error::InvalidChar(char)),
            },
            char => return Err(Error::InvalidChar(char)),
        };
        bytes.push(byte);
        code = code.trim_start();
    }
    Ok(bytes)
}
