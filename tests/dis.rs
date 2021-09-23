use dasha::{Inst, Op, Reg, Scale, Spanning};

#[test]
fn test_dasha_disasm() {
    // addb %al, (%eax)    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm(&[Spanning(0x00, 0, 1, None), Spanning(0x00, 1, 1, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 1, None),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Eax, 1, 1, Some(0b111 << 3))),
                        index: None,
                        scale: None,
                    },
                    1,
                    1,
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
        dasha::disasm(&[
            Spanning(0x00, 0, 2, None),
            Spanning(0x49, 2, 2, None),
            Spanning(0x00, 5, 2, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Cl, 2, 2, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x00, 5, 2, None)),
                        base: Some(Spanning(Reg::Ecx, 2, 2, Some(0b111))),
                        index: None,
                        scale: None,
                    },
                    2,
                    2,
                    Some(0b111),
                ),
            ),
            0,
            7,
            None,
        )]),
    );
    // addb %dl, 0x7f(%edx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x52, 1, 1, None),
            Spanning(0x7f, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Dl, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x7f, 2, 1, None)),
                        base: Some(Spanning(Reg::Edx, 1, 1, Some(0b111))),
                        index: None,
                        scale: None,
                    },
                    1,
                    1,
                    Some(0b111),
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %bl, -0x80(%ebx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x5b, 1, 1, None),
            Spanning(0x80, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Bl, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x80, 2, 1, None)),
                        base: Some(Spanning(Reg::Ebx, 1, 1, Some(0b111))),
                        index: None,
                        scale: None,
                    },
                    1,
                    1,
                    Some(0b111),
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %ah, -1(%ebx)    [opcode + mod-reg-r/m + disp8]
    assert_eq!(
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x63, 1, 1, None),
            Spanning(0xff, 2, 1, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ah, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(-0x01, 2, 1, None)),
                        base: Some(Spanning(Reg::Ebx, 1, 1, Some(0b111))),
                        index: None,
                        scale: None,
                    },
                    1,
                    1,
                    Some(0b111),
                ),
            ),
            0,
            3,
            None,
        )]),
    );
    // addb %ch, 0x7fffffff(%ebp)    [opcode + mod-reg-r/m + disp32]
    assert_eq!(
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0xad, 1, 1, None),
            Spanning(0xff, 2, 1, None),
            Spanning(0xff, 3, 1, None),
            Spanning(0xff, 4, 1, None),
            Spanning(0x7f, 5, 1, None),
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Ch, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: Some(Spanning(0x7fffffff, 2, 4, None)),
                        base: Some(Spanning(Reg::Ebp, 1, 1, Some(0b111))),
                        index: None,
                        scale: None,
                    },
                    1,
                    1,
                    Some(0b111),
                ),
            ),
            0,
            6,
            None,
        )]),
    );
    // addb %al, %al    [opcode + mod-reg-r/m]
    assert_eq!(
        dasha::disasm(&[Spanning(0x00, 0, 1, None), Spanning(0xc0, 1, 1, None)]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 1, Some(0b111 << 3)),
                Spanning(Op::Dir(Reg::Al), 1, 1, Some(0b111)),
            ),
            0,
            2,
            None,
        )]),
    );

    /// SIB addressing
    // addb %al, (%eax, %eax, 1)    [opcode + mod-reg-r/m + scale-index-base]
    assert_eq!(
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x04, 1, 1, None),
            Spanning(0x00, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Al, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Eax, 2, 1, Some(0b111))),
                        index: Some(Spanning(Reg::Eax, 2, 1, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::One, 2, 1, Some(0b11 << 6))),
                    },
                    1,
                    1,
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
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x0c, 1, 1, None),
            Spanning(0x49, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Cl, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Ecx, 2, 1, Some(0b111))),
                        index: Some(Spanning(Reg::Ecx, 2, 1, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Two, 2, 1, Some(0b11 << 6))),
                    },
                    1,
                    1,
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
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x14, 1, 1, None),
            Spanning(0x92, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Dl, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Edx, 2, 1, Some(0b111))),
                        index: Some(Spanning(Reg::Edx, 2, 1, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Four, 2, 1, Some(0b11 << 6))),
                    },
                    1,
                    1,
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
        dasha::disasm(&[
            Spanning(0x00, 0, 1, None),
            Spanning(0x1c, 1, 1, None),
            Spanning(0xdb, 2, 1, None)
        ]),
        Ok(vec![Spanning(
            Inst::AddRegOp(
                Spanning(Reg::Bl, 1, 1, Some(0b111 << 3)),
                Spanning(
                    Op::Ind {
                        disp: None,
                        base: Some(Spanning(Reg::Ebx, 2, 1, Some(0b111))),
                        index: Some(Spanning(Reg::Ebx, 2, 1, Some(0b111 << 3))),
                        scale: Some(Spanning(Scale::Eight, 2, 1, Some(0b11 << 6))),
                    },
                    1,
                    1,
                    None,
                ),
            ),
            0,
            3,
            None,
        )]),
    );

    // TODO: addb %al, (%esp)
}
