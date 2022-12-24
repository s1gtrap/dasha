use std::fmt;

pub(crate) mod dis;
pub(crate) mod display;
pub mod text;

pub use dis::{disasm_bytes, Error};
pub use text::{Loc, Spanning};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Size {
    Byte,
    Word,
    Long,
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Size::Byte => write!(f, "b"),
            Size::Word => write!(f, "w"),
            Size::Long => write!(f, "l"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scale {
    One,
    Two,
    Four,
    Eight,
}

impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Scale::One => write!(f, "1"),
            Scale::Two => write!(f, "2"),
            Scale::Four => write!(f, "4"),
            Scale::Eight => write!(f, "8"),
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Scale {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}", self))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg {
    // 8-bit registers
    Al,
    Cl,
    Dl,
    Bl,
    Ah,
    Ch,
    Dh,
    Bh,

    // 32-bit registers
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
}

impl Reg {
    pub fn size(self) -> Size {
        match self {
            Reg::Al | Reg::Cl | Reg::Dl | Reg::Bl | Reg::Ah | Reg::Ch | Reg::Dh | Reg::Bh => {
                Size::Byte
            }
            Reg::Eax
            | Reg::Ecx
            | Reg::Edx
            | Reg::Ebx
            | Reg::Esp
            | Reg::Ebp
            | Reg::Esi
            | Reg::Edi => Size::Long,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op<L>
where
    L: Clone,
{
    Dir(Reg),
    Ind {
        disp: Option<Spanning<i128, L>>,
        base: Option<Spanning<Reg, L>>,
        index: Option<Spanning<Reg, L>>,
        scale: Option<Spanning<Scale, L>>,
        size: Size,
    },
}

impl<L> Op<L>
where
    L: Clone,
{
    pub fn size(&self) -> Size {
        match self {
            Op::Dir(reg) => reg.size(),
            Op::Ind { size, .. } => *size,
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::Al => write!(f, "%al"),
            Reg::Cl => write!(f, "%cl"),
            Reg::Dl => write!(f, "%dl"),
            Reg::Bl => write!(f, "%bl"),
            Reg::Ah => write!(f, "%ah"),
            Reg::Ch => write!(f, "%ch"),
            Reg::Dh => write!(f, "%dh"),
            Reg::Bh => write!(f, "%bh"),
            Reg::Eax => write!(f, "%eax"),
            Reg::Ecx => write!(f, "%ecx"),
            Reg::Edx => write!(f, "%edx"),
            Reg::Ebx => write!(f, "%ebx"),
            Reg::Esp => write!(f, "%esp"),
            Reg::Ebp => write!(f, "%ebp"),
            Reg::Esi => write!(f, "%esi"),
            Reg::Edi => write!(f, "%edi"),
        }
    }
}

impl<L> fmt::Display for Op<L>
where
    L: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Dir(reg) => write!(f, "{}", reg),
            Op::Ind {
                disp: None,
                base: None,
                index: None,
                scale: None,
                ..
            } => unreachable!(),
            Op::Ind {
                disp: None,
                base: Some(Spanning(base, _, _, _)),
                index: None,
                scale: None,
                ..
            } => write!(f, "({})", base),
            Op::Ind {
                disp: Some(Spanning(disp, _, _, _)),
                base: Some(Spanning(base, _, _, _)),
                index: None,
                scale: None,
                ..
            } => write!(f, "{}({})", disp, base),
            Op::Ind {
                disp: None,
                base: Some(Spanning(base, _, _, _)),
                index: Some(Spanning(index, _, _, _)),
                scale: Some(Spanning(scale, _, _, _)),
                ..
            } => write!(f, "({}, {}, {})", base, index, scale),
            Op::Ind {
                disp: Some(Spanning(disp, _, _, _)),
                base: Some(Spanning(base, _, _, _)),
                index: Some(Spanning(index, _, _, _)),
                scale: Some(Spanning(scale, _, _, _)),
                ..
            } => write!(f, "{}({}, {}, {})", disp, base, index, scale),
            op => unimplemented!("{:?}", op),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst<L>
where
    L: fmt::Debug + Clone,
{
    AddRegOp(Spanning<Reg, L>, Spanning<Op<L>, L>),
    AddOpReg(Spanning<Op<L>, L>, Spanning<Reg, L>),
    AddImmReg(Spanning<i128, L>, Spanning<Reg, L>),
}

impl<L> Inst<L>
where
    L: fmt::Debug + Clone,
{
    pub fn size(&self) -> Size {
        match self {
            Inst::AddRegOp(_, ref op) | Inst::AddOpReg(ref op, _) => op.0.size(),
            Inst::AddImmReg(_, reg) => reg.0.size(),
        }
    }
}

impl<L> fmt::Display for Inst<L>
where
    L: fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::AddRegOp(src, dst) => write!(f, "add{} {}, {}", dst.0.size(), src, dst),
            Inst::AddOpReg(src, dst) => write!(f, "add{} {}, {}", src.0.size(), src, dst),
            Inst::AddImmReg(src, dst) => write!(f, "add{} {}, {}", dst.0.size(), src, dst),
        }
    }
}

pub fn disasm(code: &str) -> Result<Vec<Spanning<Inst<Loc>, Loc>>, Error<Loc>> {
    dis::disasm_bytes(text::tokenize(code)?)
}
