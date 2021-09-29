#![feature(destructuring_assignment)]

use std::fmt;

mod dis;

pub use dis::{disasm, Error};

#[derive(Clone)]
pub struct Spanning<T>(pub T, pub usize, pub usize, pub Option<u8>)
where
    T: Clone;

impl<T> PartialEq for Spanning<T>
where
    T: Clone + PartialEq,
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

#[derive(Clone, Copy, Debug)]
pub enum Size {
    Byte,
    Word,
    Long,
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

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Dir(Reg),
    Ind {
        disp: Option<Spanning<i128>>,
        base: Option<Spanning<Reg>>,
        index: Option<Spanning<Reg>>,
        scale: Option<Spanning<Scale>>,
    },
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
            } => unreachable!(),
            Op::Ind {
                disp: None,
                base: Some(Spanning(base, _, _, _)),
                index: None,
                scale: None,
            } => write!(f, "({})", base),
            Op::Ind {
                disp: Some(Spanning(disp, _, _, _)),
                base: Some(Spanning(base, _, _, _)),
                index: None,
                scale: None,
            } => write!(f, "{}({})", disp, base),
            Op::Ind {
                disp: None,
                base: Some(Spanning(base, _, _, _)),
                index: Some(Spanning(index, _, _, _)),
                scale: Some(Spanning(scale, _, _, _)),
            } => write!(f, "({}, {}, {})", base, index, scale),
            Op::Ind {
                disp: Some(Spanning(disp, _, _, _)),
                base: Some(Spanning(base, _, _, _)),
                index: Some(Spanning(index, _, _, _)),
                scale: Some(Spanning(scale, _, _, _)),
            } => write!(f, "{}({}, {}, {})", disp, base, index, scale),
            op => unimplemented!("{:?}", op),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    AddRegOp(Spanning<Reg>, Spanning<Op>),
    AddOpReg(Spanning<Op>, Spanning<Reg>),
    AddImmReg(Spanning<i128>, Spanning<Reg>),
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::AddRegOp(src, dst) => write!(f, "add {}, {}", src, dst),
            Inst::AddOpReg(src, dst) => write!(f, "add {}, {}", src, dst),
            Inst::AddImmReg(src, dst) => write!(f, "add {}, {}", src, dst),
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
            } => unreachable!(),
            Op::Ind {
                disp: Some(disp),
                base: Some(base),
                index: None,
                scale: None,
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
            },
            2,
            1,
            None,
        ))
        .unwrap(),
        r#"{"span":[2,1],"child":["(",{"span":[2,1,7],"child":"%ebx"},",",{"span":[2,1,56],"child":"%ebx"},",",{"span":[2,1,192],"child":"1"},")"]}"#,
    );
}
