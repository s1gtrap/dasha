use dasha::{Inst, Loc, Op, Reg, Scale, Size, Spanning};

use pretty_assertions::{assert_eq, assert_ne};

#[test]
fn test_dasha_disasm_bytes() {
    // addb %al, (%eax)    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm_bytes([Spanning(0x00, 0, 1, None), Spanning(0x00, 1, 2, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Eax, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            2,
            None,
        )]),
    );
    // addb %cl, 0x0(%ecx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 2, None),
            Spanning(0x49, 2, 4, None),
            Spanning(0x00, 5, 7, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Cl, 2, 4, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x00, 5, 7, None)),
                        base: Some(Spanning(Reg::Ecx, 2, 4, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    2,
                    4,
                    None,
                ),
            ),
            0,
            7,
            None,
        )]),
    );
    // addb %dl, 0x7f(%edx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x52, 1, 2, None),
            Spanning(0x7f, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Dl, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x7f, 2, 3, None)),
                        base: Some(Spanning(Reg::Edx, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %bl, -0x80(%ebx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x5b, 1, 2, None),
            Spanning(0x80, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Bl, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x80, 2, 3, None)),
                        base: Some(Spanning(Reg::Ebx, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %ah, -1(%ebx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x63, 1, 2, None),
            Spanning(0xff, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ah, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x01, 2, 3, None)),
                        base: Some(Spanning(Reg::Ebx, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %ch, 0x7fffffff(%ebp)    [opcode + mod-reg-r/m + disp32]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0xad, 1, 2, None),
            Spanning(0xff, 2, 3, None),
            Spanning(0xff, 3, 4, None),
            Spanning(0xff, 4, 5, None),
            Spanning(0x7f, 5, 6, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ch, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x7fffffff, 2, 6, None)),
                        base: Some(Spanning(Reg::Ebp, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            6,
            None,
        )]),
    );
    // addb %al, %al    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm_bytes([Spanning(0x00, 0, 1, None), Spanning(0xc0, 1, 2, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 2, Some(0b111 << 3)),
                Spanning(Op::Dir(Reg::Al), 1, 2, Some(0b111)),
            ),
            0,
            2,
            None,
        )]),
    );

    /* SIB addressing */
    // addb %al, (%eax, %eax, 1)    [opcode + mod-reg-r/m + scale-index-base]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x04, 1, 2, None),
            Spanning(0x00, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Eax, 2, 3, Some(0b111))),
                        index: Some(Spanning(Reg::Eax, 2, 3, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::One, 2, 3, Some(0b11 << 6))),
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %cl, (%ecx, %ecx, 2)    [opcode + mod-reg-r/m + scale-index-base]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x0c, 1, 2, None),
            Spanning(0x49, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Cl, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Ecx, 2, 3, Some(0b111))),
                        index: Some(Spanning(Reg::Ecx, 2, 3, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Two, 2, 3, Some(0b11 << 6))),
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %dl, (%edx, %edx, 4)    [opcode + mod-reg-r/m + scale-index-base]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x14, 1, 2, None),
            Spanning(0x92, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Dl, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Edx, 2, 3, Some(0b111))),
                        index: Some(Spanning(Reg::Edx, 2, 3, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Four, 2, 3, Some(0b11 << 6))),
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %bl, (%ebx, %ebx, 8)    [opcode + mod-reg-r/m + scale-index-base]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x00, 0, 1, None),
            Spanning(0x1c, 1, 2, None),
            Spanning(0xdb, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Bl, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Ebx, 2, 3, Some(0b111))),
                        index: Some(Spanning(Reg::Ebx, 2, 3, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Eight, 2, 3, Some(0b11 << 6))),
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addl %eax, %eax    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm_bytes([Spanning(0x01, 0, 1, None), Spanning(0x00, 1, 2, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Eax, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Eax, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Long,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            2,
            None,
        )]),
    );
    // addl %ecx, 0x0(%ecx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x01, 0, 1, None),
            Spanning(0x49, 1, 2, None),
            Spanning(0x00, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ecx, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x00, 2, 3, None)),
                        base: Some(Spanning(Reg::Ecx, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Long,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addl %edx, 0x7fffffff(%edx)    [opcode + mod-reg-r/m + disp32]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x01, 0, 1, None),
            Spanning(0x92, 1, 2, None),
            Spanning(0xff, 2, 3, None),
            Spanning(0xff, 3, 4, None),
            Spanning(0xff, 4, 5, None),
            Spanning(0x7f, 5, 6, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Edx, 1, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x7fffffff, 2, 6, None)),
                        base: Some(Spanning(Reg::Edx, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Long,
                    },
                    1,
                    2,
                    None,
                ),
            ),
            0,
            6,
            None,
        )]),
    );
    // addl %ebx, %ebx    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm_bytes([Spanning(0x01, 0, 1, None), Spanning(0xdb, 1, 2, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ebx, 1, 2, Some(0b111 << 3)),
                Spanning(Op::Dir(Reg::Ebx), 1, 2, Some(0b111)),
            ),
            0,
            2,
            None,
        )]),
    );
    // addb -0x80(%esi), %al    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x02, 0, 1, None),
            Spanning(0x46, 1, 2, None),
            Spanning(0x80, 2, 3, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddOpReg(
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x80, 2, 3, None)),
                        base: Some(Spanning(Reg::Esi, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Byte,
                    },
                    1,
                    2,
                    None,
                ),
                Spanning(Reg::Al, 1, 2, Some(0b111 << 3)),
            ),
            0,
            3,
            None,
        )]),
    );
    // addl -1(%edi), %eax    [opcode + mod-reg-r/m + disp32]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x03, 0, 1, None),
            Spanning(0x87, 1, 2, None),
            Spanning(0xff, 2, 3, None),
            Spanning(0xff, 3, 4, None),
            Spanning(0xff, 4, 5, None),
            Spanning(0xff, 5, 6, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddOpReg(
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x1, 2, 6, None)),
                        base: Some(Spanning(Reg::Edi, 1, 2, Some(0b111))),
                        index: None,
                        scale: None,
                        size: Size::Long,
                    },
                    1,
                    2,
                    None,
                ),
                Spanning(Reg::Eax, 1, 2, Some(0b111 << 3)),
            ),
            0,
            6,
            None,
        )]),
    );
    // addb $-0x80, %al    [opcode + imm8]
    assert_eq!(
        dasha::disasm_bytes([
            Spanning(0x04, Loc(1, 1), Loc(1, 3), None),
            Spanning(0x80, Loc(1, 3), Loc(1, 5), None),
        ]),
        Ok(vec![Spanning(
            Inst::AddImmReg(
                Spanning(-0x80, Loc(1, 3), Loc(1, 5), None),
                Spanning(Reg::Al, Loc(1, 1), Loc(1, 3), None),
            ),
            Loc(1, 1),
            Loc(1, 5),
            None,
        )]),
    );

    // TODO: addb %al, (%esp)
}
