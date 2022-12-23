use std::{error, fmt};

use crate::{Inst, Loc, Op, Reg, Scale, Size, Spanning};

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

trait ByteExt<L>
where
    L: Clone,
{
    fn mode(self) -> Mode;
    fn reg(self, size: Size) -> Spanning<Reg, L>;
    fn rm(self, size: Size) -> Spanning<Reg, L>;

    fn scale(self) -> Spanning<Scale, L>;
    fn index(self, size: Size) -> Spanning<Reg, L>;
    fn base(self, size: Size) -> Spanning<Reg, L>;
}

impl<'a, L> ByteExt<L> for &'a Spanning<u8, L>
where
    L: Clone,
{
    fn mode(self) -> Mode {
        match self.0 >> 6 {
            0b00 => Mode::Indirect,
            0b01 => Mode::ByteDisp,
            0b10 => Mode::LongDisp,
            0b11 => Mode::Direct,
            _ => unreachable!(),
        }
    }

    fn reg(self, size: Size) -> Spanning<Reg, L> {
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
            self.1.clone(),
            self.2.clone(),
            Some(0b111 << 3),
        )
    }

    fn rm(self, size: Size) -> Spanning<Reg, L> {
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
            self.1.clone(),
            self.2.clone(),
            Some(0b111),
        )
    }

    fn scale(self) -> Spanning<Scale, L> {
        Spanning(
            match self.0 >> 6 {
                0b00 => Scale::One,
                0b01 => Scale::Two,
                0b10 => Scale::Four,
                0b11 => Scale::Eight,
                _ => unreachable!(),
            },
            self.1.clone(),
            self.2.clone(),
            Some(0b11 << 6),
        )
    }

    fn index(self, size: Size) -> Spanning<Reg, L> {
        self.reg(size)
    }

    fn base(self, size: Size) -> Spanning<Reg, L> {
        self.rm(size)
    }
}

trait ByteSliceExt<'a, L>
where
    L: fmt::Debug + Clone + PartialEq,
{
    fn mode(self) -> Option<Mode>;
    fn reg(self, size: Size) -> Result<Spanning<Reg, L>, Error<L>>;
    fn rm(self, size: Size) -> Result<Spanning<Op<L>, L>, Error<L>>;
    fn inst_len(self) -> Result<usize, Error<L>>;
    fn inst_split(self)
        -> Result<Option<(&'a [Spanning<u8, L>], &'a [Spanning<u8, L>])>, Error<L>>;

    fn prim<I>(self) -> Option<I>
    where
        I: Fixed<L>;
}

impl<'a, L> ByteSliceExt<'a, L> for &'a [Spanning<u8, L>]
where
    L: fmt::Debug + Clone + PartialEq,
{
    fn mode(self) -> Option<Mode> {
        self.get(0).map(|mrr| match mrr.0 >> 6 {
            0b00 => Mode::Indirect,
            0b01 => Mode::ByteDisp,
            0b10 => Mode::LongDisp,
            0b11 => Mode::Direct,
            _ => unreachable!(),
        })
    }

    fn reg(self, size: Size) -> Result<Spanning<Reg, L>, Error<L>> {
        self.get(0)
            .map(|mrr| mrr.reg(size))
            .ok_or(Error::ExpectedMrr)
    }

    fn rm(self, size: Size) -> Result<Spanning<Op<L>, L>, Error<L>> {
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
                                Spanning(disp.0 as i8 as _, disp.1.clone(), disp.2.clone(), None)
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
                                    self[1].1.clone(),
                                    self[4].2.clone(),
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
                    mrr.1.clone(),
                    mrr.2.clone(),
                    match mrr.mode() {
                        Mode::Direct => Some(0b111),
                        Mode::Indirect | Mode::ByteDisp | Mode::LongDisp => None,
                    },
                ))
            })
    }

    fn inst_len(self) -> Result<usize, Error<L>> {
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

    fn inst_split(
        self,
    ) -> Result<Option<(&'a [Spanning<u8, L>], &'a [Spanning<u8, L>])>, Error<L>> {
        let len = self.inst_len()?;
        Ok(self.get(..len).map(|head| (head, self.get(len..).unwrap())))
    }

    fn prim<I>(self) -> Option<I>
    where
        I: Fixed<L>,
    {
        I::from_slice(self)
    }
}

trait Fixed<L>: Sized
where
    L: Clone,
{
    fn from_slice(s: &[Spanning<u8, L>]) -> Option<Self>;
}

impl<L> Fixed<L> for Spanning<u8, L>
where
    L: Clone,
{
    fn from_slice(s: &[Spanning<u8, L>]) -> Option<Self> {
        s.get(0).cloned()
    }
}

impl<L> Fixed<L> for Spanning<i8, L>
where
    L: Clone,
{
    fn from_slice(s: &[Spanning<u8, L>]) -> Option<Self> {
        s.get(0)
            .cloned()
            .map(|Spanning(a, b, c, d)| Spanning(a as _, b, c, d))
    }
}

#[derive(Debug, PartialEq)]
pub enum Error<L>
where
    L: fmt::Debug + Clone + PartialEq,
{
    ExpectedMrr,
    ExpectedSib,
    ExpectedByteDisp,
    ExpectedLongDisp,
    ExpectedByteImm,
    ExpectedLongImm,
    PartialInst,
    Text(crate::text::Error),
    UnimplOpc(Spanning<u8, L>),
}

impl<L> From<crate::text::Error> for Error<L>
where
    L: fmt::Debug + Clone + PartialEq,
{
    fn from(other: crate::text::Error) -> Self {
        Error::Text(other)
    }
}

impl<L> error::Error for Error<L> where L: fmt::Debug + Clone + PartialEq {}

impl<L> fmt::Display for Error<L>
where
    L: fmt::Debug + Clone + PartialEq,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ExpectedMrr => write!(f, "expected mod-reg-r/m byte"),
            Error::ExpectedSib => write!(f, "expected sib byte"),
            Error::ExpectedByteDisp => write!(f, "expected byte disp (1 byte)"),
            Error::ExpectedLongDisp => write!(f, "expected long disp (4 bytes)"),
            Error::ExpectedByteImm => write!(f, "expected byte imm (1 byte)"),
            Error::ExpectedLongImm => write!(f, "expected long imm (4 bytes)"),
            Error::PartialInst => write!(f, "partial/incomplete instruction"),
            Error::Text(e) => write!(f, "text error: {}", e),
            Error::UnimplOpc(c) => write!(f, "unimplemented opcode: {:x?}", c.0),
        }
    }
}

fn reg_op<'a, L>(
    f: &dyn Fn(Spanning<Reg, L>, Spanning<Op<L>, L>) -> Inst<L>,
    s: &'a [Spanning<u8, L>],
    sz: Size,
) -> Result<(Spanning<Inst<L>, L>, &'a [Spanning<u8, L>]), Error<L>>
where
    L: fmt::Debug + Clone + PartialEq,
{
    match s {
        &[Spanning(_, ref oss, ref osl, _), ref tail @ ..] => Ok((
            Spanning(
                f(tail.reg(sz)?, tail.rm(sz)?),
                oss.clone(),
                tail.inst_split()?
                    .ok_or(Error::PartialInst)?
                    .0
                    .last()
                    .map(|Spanning(_, _, sl, _)| sl.clone())
                    .unwrap(),
                None,
            ),
            tail.inst_split()?.ok_or(Error::PartialInst)?.1,
        )),
        _ => unreachable!(),
    }
}

fn op_reg<'a, L>(
    f: &dyn Fn(Spanning<Op<L>, L>, Spanning<Reg, L>) -> Inst<L>,
    s: &'a [Spanning<u8, L>],
    sz: Size,
) -> Result<(Spanning<Inst<L>, L>, &'a [Spanning<u8, L>]), Error<L>>
where
    L: fmt::Debug + Clone + PartialEq,
{
    match s {
        &[Spanning(_, ref oss, ref osl, _), ref tail @ ..] => Ok((
            Spanning(
                f(tail.rm(sz)?, tail.reg(sz)?),
                oss.clone(),
                tail.inst_split()?
                    .ok_or(Error::PartialInst)?
                    .0
                    .last()
                    .map(|Spanning(_, _, sl, _)| sl.clone())
                    .unwrap_or(osl.clone()),
                None,
            ),
            tail.inst_split()?.ok_or(Error::PartialInst)?.1,
        )),
        _ => unreachable!(),
    }
}

fn imm_reg<'a, L>(
    f: &dyn Fn(Spanning<i128, L>, Spanning<Reg, L>) -> Inst<L>,
    s: &'a [Spanning<u8, L>],
    sz: Size,
) -> Result<(Spanning<Inst<L>, L>, &'a [Spanning<u8, L>]), Error<L>>
where
    L: fmt::Debug + Clone + PartialEq,
{
    match s {
        &[Spanning(_, ref oss, ref osl, _), ref tail @ ..] => Ok((
            Spanning(
                f(
                    tail.get(0)
                        .map(|Spanning(i, s, l, b)| Spanning(*i as _, s.clone(), l.clone(), *b))
                        .ok_or(Error::ExpectedByteImm)?,
                    tail.reg(sz)?,
                ),
                oss.clone(),
                tail.inst_split()?
                    .ok_or(Error::PartialInst)?
                    .0
                    .last()
                    .map(|Spanning(_, _, sl, _)| sl.clone())
                    .unwrap_or(osl.clone()),
                None,
            ),
            tail.inst_split()?.ok_or(Error::PartialInst)?.1,
        )),
        _ => unreachable!(),
    }
}

pub fn disasm_bytes<I, L>(i: I) -> Result<Vec<Spanning<Inst<L>, L>>, Error<L>>
where
    I: IntoIterator<Item = Spanning<u8, L>>,
    L: fmt::Debug + Clone + PartialEq,
{
    let mut code = &i.into_iter().collect::<Vec<_>>()[..];
    let mut insts = vec![];
    while !code.is_empty() {
        let inst;
        (inst, code) = match &code[..] {
            [Spanning(0x00, _, _, _), ..] => reg_op(&Inst::AddRegOp, code, Size::Byte)?,
            [Spanning(0x01, _, _, _), ..] => reg_op(&Inst::AddRegOp, code, Size::Long)?,
            [Spanning(0x02, _, _, _), ..] => op_reg(&Inst::AddOpReg, code, Size::Byte)?,
            [Spanning(0x03, _, _, _), ..] => op_reg(&Inst::AddOpReg, code, Size::Long)?,
            [Spanning(0x04, s, l, _), ref tail @ ..] => tail
                .prim::<Spanning<i8, L>>()
                .ok_or(Error::ExpectedByteImm)
                .map(|ref p @ Spanning(_, ref ss, ref sl, _)| -> (_, _) {
                    (
                        Spanning(
                            Inst::AddImmReg(
                                p.clone().map(|i| i as _),
                                Spanning(Reg::Al, s.clone(), l.clone(), None),
                            ),
                            s.clone(),
                            sl.clone(),
                            None,
                        ),
                        tail.get(1..).unwrap(),
                    )
                })?,
            [c, ..] => return Err(Error::UnimplOpc(c.clone())),
            [] => unreachable!(),
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

#[test]
fn test_byte_slice_ext_prim() {
    assert_eq!([].prim::<Spanning<u8, Loc>>(), None);
    assert_eq!(
        [Spanning(0x00, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<u8, _>>(),
        Some(Spanning(0x00, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0x7f, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<u8, _>>(),
        Some(Spanning(0x7f, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0x80, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<u8, _>>(),
        Some(Spanning(0x80, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0xff, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<u8, _>>(),
        Some(Spanning(0xff, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!([].as_ref().prim::<Spanning<i8, Loc>>(), None);
    assert_eq!(
        [Spanning(0x00, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<i8, _>>(),
        Some(Spanning(0x00, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0x7f, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<i8, _>>(),
        Some(Spanning(0x7f, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0x80, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<i8, _>>(),
        Some(Spanning(-0x80, Loc(1, 1), Loc(1, 2), None)),
    );
    assert_eq!(
        [Spanning(0xff, Loc(1, 1), Loc(1, 2), None)].prim::<Spanning<i8, _>>(),
        Some(Spanning(-0x1, Loc(1, 1), Loc(1, 2), None)),
    );
}
