use std::fmt;

use serde::ser::{Serialize, SerializeSeq, SerializeStruct, SerializeTuple, Serializer};

use crate::{Inst, Loc, Op, Reg, Spanning};

impl Serialize for Loc {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(1))?;
        seq.serialize_element(&self.0)?;
        seq.serialize_element(&self.1)?;
        seq.serialize_element(&self.2)?;
        seq.end()
    }
}

impl Serialize for Reg {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(1))?;
        seq.serialize_element(&format!("{}", self))?;
        seq.end()
    }
}

impl<L, T> Serialize for Spanning<T, L>
where
    L: fmt::Debug + Clone + Serialize,
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut span = serializer.serialize_struct("Span", 3)?;
        span.serialize_field("edges", &self.0)?;
        match self.3 {
            None => span.serialize_field("span", &(&self.1, &self.2))?,
            Some(mask) => span.serialize_field("span", &(&self.1, &self.2, &mask))?,
        }
        span.end()
    }
}

impl<L> Serialize for Op<L>
where
    L: fmt::Debug + Clone + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Op::Dir(reg) => serializer.serialize_some(&reg),
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
            } => {
                let mut tup = serializer.serialize_tuple(3)?;
                tup.serialize_element(&"(")?;
                tup.serialize_element(&base)?;
                tup.serialize_element(&")")?;
                tup.end()
            }
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
                index: Some(index),
                scale: Some(scale),
                ..
            } => {
                let mut tup = serializer.serialize_tuple(7)?;
                tup.serialize_element("(")?;
                tup.serialize_element(base)?;
                tup.serialize_element(",")?;
                tup.serialize_element(index)?;
                tup.serialize_element(",")?;
                tup.serialize_element(scale)?;
                tup.serialize_element(")")?;
                tup.end()
            }
            _ => todo!(),
        }
    }
}

impl<L> Serialize for Inst<L>
where
    L: fmt::Debug + Clone + Serialize,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        fn cs2<S, S1, S2>(s: S, op: &str, s1: S1, s2: S2) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
            S1: Serialize,
            S2: Serialize,
        {
            let mut tup = s.serialize_tuple(4)?;
            tup.serialize_element(&format!("{} ", op))?;
            tup.serialize_element(&s1)?;
            tup.serialize_element(&", ")?;
            tup.serialize_element(&s2)?;
            tup.end()
        }

        match self {
            Inst::AddRegOp(reg, op) => cs2(s, "addl", reg, op),
            Inst::AddOpReg(op, reg) => cs2(s, "addl", op, reg),
            Inst::AddImmReg(imm, reg) => cs2(s, "addl", imm, reg),
        }
    }
}

#[test]
#[cfg(feature = "serde")]
fn test_display_serialize() {
    use crate::Size;
    assert_eq!(
        &serde_json::to_string(&Spanning(Reg::Ecx, 0, 1, None)).unwrap(),
        r#"{"edges":["%ecx"],"span":[0,1]}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(Reg::Edx, 0, 1, None)).unwrap(),
        r#"{"edges":["%edx"],"span":[0,1]}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(Op::<usize>::Dir(Reg::Edx), 0, 1, None)).unwrap(),
        r#"{"edges":["%edx"],"span":[0,1]}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(
            Op::Ind {
                disp: None,
                base: Some(Spanning(Reg::Ebx, 2, 3, None)),
                index: None,
                scale: None,
                size: Size::Long,
            },
            1,
            2,
            None,
        ))
        .unwrap(),
        r#"{"edges":["(",{"edges":["%ebx"],"span":[2,3]},")"],"span":[1,2]}"#,
    );
    assert_eq!(
        &serde_json::to_string(&Inst::AddOpReg(
            Spanning(Op::Dir(Reg::Esi), 1, 2, Some(7)),
            Spanning(Reg::Edi, 1, 2, Some(56)),
        ))
        .unwrap(),
        r#"["addl ",{"edges":["%esi"],"span":[1,2,7]},", ",{"edges":["%edi"],"span":[1,2,56]}]"#,
    );
    assert_eq!(
        &serde_json::to_string(&Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Eax, 1, 2, Some(7)),
                Spanning(Op::Dir(Reg::Ecx), 1, 2, Some(56)),
            ),
            0,
            2,
            None,
        ))
        .unwrap(),
        r#"{"edges":["addl ",{"edges":["%eax"],"span":[1,2,7]},", ",{"edges":["%ecx"],"span":[1,2,56]}],"span":[0,2]}"#,
    );
}
