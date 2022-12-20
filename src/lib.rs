#![feature(destructuring_assignment)]

use std::fmt;

pub(crate) mod dis;
mod parent;
pub mod text;

pub use dis::{disasm_bytes, Error};
pub use parent::{Frag, Parent};

#[derive(Clone)]
pub struct Spanning<T>(pub T, pub usize, pub usize, pub Option<u8>);

impl<T> Spanning<T> {
    fn map<U>(self, f: fn(T) -> U) -> Spanning<U> {
        Spanning(f(self.0), self.1, self.2, self.3)
    }
}

impl<T> PartialEq for Spanning<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == self.1 && self.2 == other.2 && self.3 == self.3
    }
}

impl<T> fmt::Debug for Spanning<T>
where
    T: fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Spanning({:?}, {}, {}, {:?})",
            self.0, self.1, self.2, self.3
        )
    }
}

impl<T> fmt::Display for Spanning<T>
where
    T: fmt::Display + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl parent::Parent for Spanning<i128> {
    fn children(&self) -> Vec<parent::Frag> {
        vec![parent::Frag::Leaf(format!("{}", self))]
    }
}

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

impl parent::Parent for Spanning<Scale> {
    fn children(&self) -> Vec<parent::Frag> {
        vec![parent::Frag::Leaf(format!("{}", self))]
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
pub enum Op {
    Dir(Reg),
    Ind {
        disp: Option<Spanning<i128>>,
        base: Option<Spanning<Reg>>,
        index: Option<Spanning<Reg>>,
        scale: Option<Spanning<Scale>>,
        size: Size,
    },
}

impl Op {
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

impl fmt::Display for Op {
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

impl parent::Parent for Spanning<Op> {
    fn children(&self) -> Vec<parent::Frag> {
        match &self.0 {
            Op::Dir(reg) => vec![parent::Frag::Leaf(format!("{}", reg))],
            Op::Ind {
                disp: None,
                base: None,
                index: None,
                scale: None,
                ..
            } => unreachable!(),
            Op::Ind {
                disp: None,
                base: Some(base),
                index: None,
                scale: None,
                ..
            } => vec![
                parent::Frag::Leaf("(".into()),
                parent::Frag::Branch(Box::new(base.clone())),
                parent::Frag::Leaf(")".into()),
            ],
            Op::Ind {
                disp: Some(disp),
                base: Some(base),
                index: None,
                scale: None,
                ..
            } => vec![
                parent::Frag::Branch(Box::new(disp.clone())),
                parent::Frag::Leaf("(".into()),
                parent::Frag::Branch(Box::new(base.clone())),
                parent::Frag::Leaf(")".into()),
            ],
            Op::Ind {
                disp: None,
                base: Some(base),
                index: Some(index),
                scale: Some(scale),
                ..
            } => vec![
                parent::Frag::Leaf("(".into()),
                parent::Frag::Branch(Box::new(base.clone())),
                parent::Frag::Leaf(", ".into()),
                parent::Frag::Branch(Box::new(index.clone())),
                parent::Frag::Leaf(", ".into()),
                parent::Frag::Branch(Box::new(scale.clone())),
                parent::Frag::Leaf(")".into()),
            ],
            Op::Ind {
                disp: Some(Spanning(disp, _, _, _)),
                base: Some(Spanning(base, _, _, _)),
                index: Some(Spanning(index, _, _, _)),
                scale: Some(Spanning(scale, _, _, _)),
                ..
            } => vec![
                parent::Frag::Leaf("(".into()),
                parent::Frag::Leaf(format!("{}", base)),
                parent::Frag::Leaf("(".into()),
            ],
            op => unimplemented!("{:?}", op),
        }
    }
}

impl parent::Parent for Spanning<Reg> {
    fn children(&self) -> Vec<parent::Frag> {
        vec![parent::Frag::Leaf(format!("{}", self))]
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    AddRegOp(Spanning<Reg>, Spanning<Op>),
    AddOpReg(Spanning<Op>, Spanning<Reg>),
    AddImmReg(Spanning<i128>, Spanning<Reg>),
}

impl Inst {
    pub fn size(&self) -> Size {
        match self {
            Inst::AddRegOp(_, ref op) | Inst::AddOpReg(ref op, _) => op.0.size(),
            Inst::AddImmReg(_, reg) => reg.0.size(),
        }
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::AddRegOp(src, dst) => write!(f, "add{} {}, {}", dst.0.size(), src, dst),
            Inst::AddOpReg(src, dst) => write!(f, "add{} {}, {}", src.0.size(), src, dst),
            Inst::AddImmReg(src, dst) => write!(f, "add{} {}, {}", dst.0.size(), src, dst),
        }
    }
}

impl parent::Parent for Spanning<Inst> {
    fn children(&self) -> Vec<parent::Frag> {
        match &self.0 {
            Inst::AddRegOp(src, dst) => vec![
                parent::Frag::Leaf(format!("add{} ", dst.0.size())),
                parent::Frag::Branch(Box::new(src.clone())),
                parent::Frag::Leaf(", ".into()),
                parent::Frag::Branch(Box::new(dst.clone())),
            ],
            Inst::AddOpReg(src, dst) => vec![parent::Frag::Leaf(format!("add{}", src.0.size()))],
            Inst::AddImmReg(src, dst) => vec![parent::Frag::Leaf(format!("add{}", dst.0.size()))],
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Reg {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}", self))
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Op {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeTuple;
        match self {
            Op::Dir(reg) => reg.serialize(serializer),
            Op::Ind {
                disp: None,
                base: None,
                index: None,
                scale: None,
                ..
            } => unreachable!(),
            Op::Ind {
                disp: Some(disp),
                base: Some(base),
                index: None,
                scale: None,
                ..
            } => {
                let mut tup = serializer.serialize_tuple(4)?;
                tup.serialize_element(disp)?;
                tup.serialize_element("(")?;
                tup.serialize_element(base)?;
                tup.serialize_element(")")?;
                tup.end()
            }
            Op::Ind {
                disp: None,
                base: Some(base),
                index: None,
                scale: None,
                ..
            } => {
                let mut tup = serializer.serialize_tuple(3)?;
                tup.serialize_element("(")?;
                tup.serialize_element(base)?;
                tup.serialize_element(")")?;
                tup.end()
            }
            Op::Ind {
                disp: None,
                base: Some(base),
                index: Some(index),
                scale: Some(scale),
                ..
            } => {
                let mut tup = serializer.serialize_tuple(3)?;
                tup.serialize_element("(")?;
                tup.serialize_element(base)?;
                tup.serialize_element(",")?;
                tup.serialize_element(index)?;
                tup.serialize_element(",")?;
                tup.serialize_element(scale)?;
                tup.serialize_element(")")?;
                tup.end()
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Inst {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeTuple;
        match self {
            Inst::AddRegOp(reg, op) => {
                let mut tup = serializer.serialize_tuple(4)?;
                tup.serialize_element("add ")?;
                tup.serialize_element(reg)?;
                tup.serialize_element(", ")?;
                tup.serialize_element(op)?;
                tup.end()
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(feature = "serde")]
impl<T> serde::Serialize for Spanning<T>
where
    T: serde::Serialize + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut st = serializer.serialize_struct("", 2)?;
        st.serialize_field(
            "span",
            &[self.1, self.2]
                .iter()
                .chain(&self.3.map(|mask| mask as _))
                .collect::<Vec<_>>(),
        )?;
        st.serialize_field("child", &self.0)?;
        st.end()
    }
}

#[test]
#[cfg(feature = "serde")]
fn test_serialize() {
    assert_eq!(
        &serde_json::to_string(&Spanning(Reg::Eax, 1, 1, Some(0b00111000))).unwrap(),
        r#"{"span":[1,1,56],"child":"%eax"}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(Op::Dir(Reg::Ecx), 1, 1, Some(0b00000111))).unwrap(),
        r#"{"span":[1,1,7],"child":"%ecx"}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(
            Op::Ind {
                disp: None,
                base: Some(Spanning(Reg::Edx, 1, 1, Some(0b00000111))),
                index: None,
                scale: None,
                size: Size::Byte,
            },
            1,
            1,
            None,
        ))
        .unwrap(),
        r#"{"span":[1,1],"child":["(",{"span":[1,1,7],"child":"%edx"},")"]}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(
            Op::Ind {
                disp: None,
                base: Some(Spanning(Reg::Ebx, 2, 1, Some(0b111))),
                index: Some(Spanning(Reg::Ebx, 2, 1, Some(0b111 << 3))),
                scale: Some(Spanning(Scale::One, 2, 1, Some(0b11 << 6))),
                size: Size::Byte,
            },
            2,
            1,
            None,
        ))
        .unwrap(),
        r#"{"span":[2,1],"child":["(",{"span":[2,1,7],"child":"%ebx"},",",{"span":[2,1,56],"child":"%ebx"},",",{"span":[2,1,192],"child":"1"},")"]}"#,
    );
}

pub fn disasm(code: &str) -> Result<Vec<Spanning<Inst>>, Error> {
    dis::disasm_bytes(text::tokenize(code)?)
}
