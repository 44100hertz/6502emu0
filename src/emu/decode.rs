// Mode implies width! Please do not store width, just mode.
// Write a simple function to map one to the other, or maybe just hardcode it into decoding.

use super::{Op, StandardOp, SpecialOp, Amode, StatFlag};
use std::mem::transmute;

pub fn decode(code: u8) -> (Op, Amode) {
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
        return (op, Rela);
    }

    // special codes
    if code & 0x9F == 0x0 ||
        code & 0x0f == 0x8 ||
        code & 0x0f == 0xA && code >= 0x80
    {
        let mode = if code == SpecialOp::Jsr as u8 {
            Abs
        } else {
            Nothing
        };
        return (Op::Special(code), mode);
    }

    fn decode_standard(code: u8) -> (u8, Amode) {
        // standard (variable) codes
        let op = code & 0b1110_0011;
        let addr_bits = code >> 2 & 0b111;
        let code_bits = code & 0b11;
        let mode = match addr_bits {
            0 => match code_bits {
                1 => Idrx,
                _ => Immed,
            }
            1 => Zp,
            2 => match code_bits {
                1 => Immed,
                2 => Accum,
                _ => Error,
            }
            3 => Abs,
            4 => match code_bits {
                1 => Idry,
                _ => Error,
            }
            5 => match unsafe { transmute(op) } {
                StandardOp::Stx | StandardOp::Ldx => Zpy,
                _ => Zpx,
            }
            6 => match code_bits {
                1 => Absy,
                _ => Error,
            }
            7 => match unsafe { transmute(op) } {
                StandardOp::Ldx => Absy,
                _ => Absx,
            }
            _ => unreachable!(),
        };
        if let Error = mode {
            eprintln!("Error decoding: {:02x}", code);
        }
        (op, mode)
    }

    if code & 0b11 == 0b11 { // undocumented two-at-once codes
        let (op1, mode) = decode_standard(code & 0xfd);
        let (op2, _) = decode_standard(code & 0xfe);
//        println!("simul {:02x} with mode: {:?}", code, mode);
        (self::Op::Standard(op1, Some(op2)), mode)
    } else {
        let (op, mode) = decode_standard(code);
        (self::Op::Standard(op, None), mode)
    }
}

