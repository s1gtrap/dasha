use std::{error, fmt};

use super::{Inst, Op, Reg, Scale, Size, Spanning};

// addr mode
enum Mode {
    // 00
    Indirect,
    // 01
    ByteDisp,
    // 10
    LongDisp,
    // 11
    Direct,
}

trait ByteExt {
    fn mode(self) -> Mode;
    fn reg(self, size: Size) -> Spanning<Reg>;
    fn rm(self, size: Size) -> Spanning<Reg>;

    fn scale(self) -> Spanning<Scale>;
    fn index(self, size: Size) -> Spanning<Reg>;
    fn base(self, size: Size) -> Spanning<Reg>;
}

impl<'a> ByteExt for &'a Spanning<u8> {
    fn mode(self) -> Mode {
        match self.0 >> 6 {
            0b00 => Mode::Indirect,
            0b01 => Mode::ByteDisp,
            0b10 => Mode::LongDisp,
            0b11 => Mode::Direct,
            _ => unreachable!(),
        }
    }

    fn reg(self, size: Size) -> Spanning<Reg> {
        Spanning(
            match (self.0 >> 3 & 0b111, size) {
                (0b000, Size::Byte) => Reg::Al,
                (0b001, Size::Byte) => Reg::Cl,
                (0b010, Size::Byte) => Reg::Dl,
                (0b011, Size::Byte) => Reg::Bl,
                (0b100, Size::Byte) => Reg::Ah,
                (0b101, Size::Byte) => Reg::Ch,
                (0b110, Size::Byte) => Reg::Dh,
                (0b111, Size::Byte) => Reg::Bh,
                (0b000, Size::Long) => Reg::Eax,
                (0b001, Size::Long) => Reg::Ecx,
                (0b010, Size::Long) => Reg::Edx,
                (0b011, Size::Long) => Reg::Ebx,
                (0b100, Size::Long) => Reg::Esp,
                (0b101, Size::Long) => Reg::Ebp,
                (0b110, Size::Long) => Reg::Esi,
                (0b111, Size::Long) => Reg::Edi,
                _ => unreachable!(),
            },
            self.1,
            self.2,
            Some(0b111 << 3),
        )
    }

    fn rm(self, size: Size) -> Spanning<Reg> {
        Spanning(
            match (self.0 & 0b111, size) {
                (0b000, Size::Byte) => Reg::Al,
                (0b001, Size::Byte) => Reg::Cl,
                (0b010, Size::Byte) => Reg::Dl,
                (0b011, Size::Byte) => Reg::Bl,
                (0b100, Size::Byte) => Reg::Ah,
                (0b101, Size::Byte) => Reg::Ch,
                (0b110, Size::Byte) => Reg::Dh,
                (0b111, Size::Byte) => Reg::Bh,
                (0b000, Size::Long) => Reg::Eax,
                (0b001, Size::Long) => Reg::Ecx,
                (0b010, Size::Long) => Reg::Edx,
                (0b011, Size::Long) => Reg::Ebx,
                (0b100, Size::Long) => Reg::Esp,
                (0b101, Size::Long) => Reg::Ebp,
                (0b110, Size::Long) => Reg::Esi,
                (0b111, Size::Long) => Reg::Edi,
                _ => unreachable!(),
            },
            self.1,
            self.2,
            Some(0b111),
        )
    }

    fn scale(self) -> Spanning<Scale> {
        Spanning(
            match self.0 >> 6 {
                0b00 => Scale::One,
                0b01 => Scale::Two,
                0b10 => Scale::Four,
                0b11 => Scale::Eight,
                _ => unreachable!(),
            },
            self.1,
            self.2,
            Some(0b11 << 6),
        )
    }

    fn index(self, size: Size) -> Spanning<Reg> {
        self.reg(size)
    }

    fn base(self, size: Size) -> Spanning<Reg> {
        self.rm(size)
    }
}

trait ByteSliceExt<'a> {
    fn mode(self) -> Option<Mode>;
    fn reg(self, size: Size) -> Result<Spanning<Reg>, Error>;
    fn rm(self, size: Size) -> Result<Spanning<Op>, Error>;
    fn inst_len(self) -> Result<usize, Error>;
    fn inst_split(self) -> Result<Option<(&'a [Spanning<u8>], &'a [Spanning<u8>])>, Error>;
}

impl<'a> ByteSliceExt<'a> for &'a [Spanning<u8>] {
    fn mode(self) -> Option<Mode> {
        self.get(0).map(|mrr| match mrr.0 >> 6 {
            0b00 => Mode::Indirect,
            0b01 => Mode::ByteDisp,
            0b10 => Mode::LongDisp,
            0b11 => Mode::Direct,
            _ => unreachable!(),
        })
    }

    fn reg(self, size: Size) -> Result<Spanning<Reg>, Error> {
        self.get(0)
            .map(|mrr| mrr.reg(size))
            .ok_or(Error::ExpectedMrr)
    }

    fn rm(self, size: Size) -> Result<Spanning<Op>, Error> {
        self.get(0)
            .ok_or(Error::ExpectedMrr)
            .and_then(|mrr| -> Result<_, _> {
                Ok(Spanning(
                    match mrr.mode() {
                        Mode::Indirect if mrr.rm(Size::Long).0 == Reg::Esp => {
                            self.get(1).ok_or(Error::ExpectedSib).map(|sib| Op::Ind {
                                disp: None,
                                base: Some(sib.base(Size::Long)),
                                index: Some(sib.index(Size::Long)),
                                scale: Some(sib.scale()),
                                size,
                            })?
                        }
                        Mode::Indirect => Op::Ind {
                            disp: None,
                            base: Some(mrr.rm(Size::Long)),
                            index: None,
                            scale: None,
                            size,
                        },
                        Mode::ByteDisp => Op::Ind {
                            disp: Some({
                                let disp = self.get(1).ok_or(Error::ExpectedByteDisp)?;
                                Spanning(disp.0 as i8 as _, disp.1, disp.2, None)
                            }),
                            base: Some(mrr.rm(Size::Long)),
                            index: None,
                            scale: None,
                            size,
                        },
                        Mode::LongDisp => Op::Ind {
                            disp: Some({
                                let disp = self
                                    .get(1..5)
                                    .ok_or(Error::ExpectedLongDisp)?
                                    .iter()
                                    .map(|Spanning(byte, _, _, _)| *byte)
                                    .collect::<Vec<_>>();
                                let mut long = [0; 4];
                                long.copy_from_slice(&disp);
                                Spanning(
                                    i32::from_le_bytes(long) as _,
                                    self[1].1,
                                    self[4].1 - self[1].1 + self[4].2,
                                    None,
                                )
                            }),
                            base: Some(mrr.rm(Size::Long)),
                            index: None,
                            scale: None,
                            size,
                        },
                        Mode::Direct => Op::Dir(mrr.rm(size).0),
                    },
                    mrr.1,
                    mrr.2,
                    Some(0b111),
                ))
            })
    }

    fn inst_len(self) -> Result<usize, Error> {
        let mrr = self.get(0).ok_or(Error::ExpectedMrr)?;
        match mrr.mode() {
            Mode::Indirect if mrr.rm(Size::Long).0 == Reg::Esp => Ok(2),
            Mode::Indirect if mrr.rm(Size::Long).0 == Reg::Ebp => Ok(5),
            Mode::Indirect => Ok(1),
            Mode::ByteDisp if mrr.rm(Size::Long).0 == Reg::Esp => Ok(3),
            Mode::ByteDisp => Ok(2),
            Mode::LongDisp if mrr.rm(Size::Long).0 == Reg::Esp => Ok(6),
            Mode::LongDisp => Ok(5),
            Mode::Direct => Ok(1),
        }
    }

    fn inst_split(self) -> Result<Option<(&'a [Spanning<u8>], &'a [Spanning<u8>])>, Error> {
        let len = self.inst_len()?;
        Ok(self.get(..len).map(|head| (head, self.get(len..).unwrap())))
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ExpectedMrr,
    ExpectedSib,
    ExpectedByteDisp,
    ExpectedLongDisp,
    PartialInst,
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ExpectedMrr => write!(f, "expected mod-reg-r/m byte"),
            Error::ExpectedSib => write!(f, "expected sib byte"),
            Error::ExpectedByteDisp => write!(f, "expected byte disp (1 byte)"),
            Error::ExpectedLongDisp => write!(f, "expected long disp (4 bytes)"),
            Error::PartialInst => write!(f, "partial/incomplete instruction"),
        }
    }
}

fn reg_op<'a>(
    f: &dyn Fn(Spanning<Reg>, Spanning<Op>) -> Inst,
    s: &'a [Spanning<u8>],
) -> Result<(Spanning<Inst>, &'a [Spanning<u8>]), Error> {
    match s {
        &[Spanning(_, oss, osl, _), ref tail @ ..] => Ok((
            Spanning(
                f(tail.reg(Size::Byte)?, tail.rm(Size::Byte)?),
                oss,
                tail.inst_split()?
                    .ok_or(Error::PartialInst)?
                    .0
                    .last()
                    .map(|Spanning(_, ss, sl, _)| ss + sl - oss)
                    .unwrap_or(osl),
                None,
            ),
            tail.inst_split()?.ok_or(Error::PartialInst)?.1,
        )),
        _ => unreachable!(),
    }
}

pub fn disasm<I>(i: I) -> Result<Vec<Spanning<Inst>>, Error>
where
    I: IntoIterator<Item = Spanning<u8>>,
{
    let mut code = &i.into_iter().collect::<Vec<_>>()[..];
    let mut insts = vec![];
    while !code.is_empty() {
        let inst;
        (inst, code) = match &code[..] {
            [Spanning(0x00, _, _, _), ..] => reg_op(&Inst::AddRegOp, code)?,
            _ => unimplemented!("{:?}", code),
        };
        insts.push(inst);
    }
    Ok(insts)
}

#[test]
fn test_byte_slice_ext_inst_len() {
    assert_eq!([Spanning(0x00, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
    assert_eq!([Spanning(0x04, 0, 2, None)].as_ref().inst_len(), Ok(2)); // mod-reg-r/m + sib
    assert_eq!([Spanning(0x05, 0, 2, None)].as_ref().inst_len(), Ok(5)); // disp32 only
    assert_eq!([Spanning(0x3c, 0, 2, None)].as_ref().inst_len(), Ok(2)); // mod-reg-r/m + sib
    assert_eq!([Spanning(0x3d, 0, 2, None)].as_ref().inst_len(), Ok(5)); // disp32 only
    assert_eq!([Spanning(0x3f, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
    assert_eq!([Spanning(0x40, 0, 2, None)].as_ref().inst_len(), Ok(2)); // mod-reg-r/m + disp8
    assert_eq!([Spanning(0x44, 0, 2, None)].as_ref().inst_len(), Ok(3)); // mod-reg-r/m + sib + disp8
    assert_eq!([Spanning(0x7c, 0, 2, None)].as_ref().inst_len(), Ok(3)); // mod-reg-r/m + sib + disp8
    assert_eq!([Spanning(0x7f, 0, 2, None)].as_ref().inst_len(), Ok(2)); // mod-reg-r/m + disp8
    assert_eq!([Spanning(0x80, 0, 2, None)].as_ref().inst_len(), Ok(5)); // mod-reg-r/m + disp32
    assert_eq!([Spanning(0x84, 0, 2, None)].as_ref().inst_len(), Ok(6)); // mod-reg-r/m + sib + disp32
    assert_eq!([Spanning(0xbc, 0, 2, None)].as_ref().inst_len(), Ok(6)); // mod-reg-r/m + sib + disp32
    assert_eq!([Spanning(0xbf, 0, 2, None)].as_ref().inst_len(), Ok(5)); // mod-reg-r/m + disp32
    assert_eq!([Spanning(0xc0, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
    assert_eq!([Spanning(0xc4, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
    assert_eq!([Spanning(0xfc, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
    assert_eq!([Spanning(0xff, 0, 2, None)].as_ref().inst_len(), Ok(1)); // mod-reg-r/m
}
