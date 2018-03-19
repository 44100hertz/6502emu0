use super::{Op, StandardOp, SpecialOp, Amode, StatFlag};
use std::mem::transmute;

pub fn decode(code: u8) -> (u8, Op, Amode) {
    use self::Amode::*;

    // branch codes
    if code & 0x1f == 0x10 {
        let op = Op::Branch(
            match code >> 6 & 0x3 {
                0 => StatFlag::S,
                1 => StatFlag::V,
                2 => StatFlag::C,
                3 => StatFlag::Z,
                _ => unreachable!(),
            },
            code >> 5 & 1 != 0, // true means test against 0
        );
        return (1, op, Rela);
    }

    // special codes
    if code & 0x9F == 0x0 ||
        code & 0x0f == 0x8 ||
        code & 0x0f == 0xA && code >= 0x80
    {
        let (width, mode) = if code == SpecialOp::Jsr as u8 {
            (2, Abs)
        } else {
            (0, Nothing)
        };
        return (width, Op::Special(code), mode);
    }

    // standard (variable) codes
    let op = code & 0b11100011;
    let addr_bits = code >> 2 & 7;
    let code_bits = code & 3;

    let (width, mode) = match addr_bits {
        0 => match code_bits {
            1 => (1, Idrx),
            _ => (1, Immed),
        }
        1 => (1, Zp),
        2 => match code_bits {
            1 => (1, Immed),
            2 => (0, Accum),
            _ => (0, Error),
        }
        3 => (2, Abs),
        4 => match code_bits {
            1 => (1, Idry),
            _ => (0, Error),
        }
        5 => match unsafe { transmute(op) } {
            StandardOp::Stx | StandardOp::Ldx => (1, Zpy),
            _ => (1, Zpx),
        }
        6 => match code_bits {
            1 => (2, Absy),
            _ => (0, Error),
        }
        7 => match unsafe { transmute(op) } {
            StandardOp::Ldx => (2, Absy),
            _ => (2, Absx),
        }
        _ => unreachable!(),
    };

    (width, self::Op::Standard(op), mode)
}
