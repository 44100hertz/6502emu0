mod decode;
mod chip;

pub fn run(rom: Rom) {
    assert!(rom.rom.len() <= 0x8000, "exceeded maximum ROM size");
    chip::Chip::new(rom).run();
}

// Variable opcodes with middle (argument) bits ___XXX__ set to 0
// http://www.llx.com/~nparker/a2/opcodes.html
#[allow(dead_code)]
#[repr(u8)]
pub enum StandardOp {
    Ora = 0x01, Asl,
    Bit = 0x20, And, Rol,
    Jmp = 0x40, Eor, Lsr,
    Jma = 0x60, Adc, Ror,
    Sty = 0x80, Sta, Stx,
    Ldy = 0xA0, Lda, Ldx,
    Cpy = 0xC0, Cmp, Dec,
    Cpx = 0xE0, Sbc, Inc,
}
// Single-kinded opcodes best to list invidiually
#[allow(dead_code)]
#[repr(u8)]
pub enum SpecialOp {
    // _8
    Php = 0x08, Plp = 0x28, Pha = 0x48, Pla = 0x68,
    Dey = 0x88, Tay = 0xA8, Iny = 0xC8, Inx = 0xE8,
    Clc = 0x18, Sec = 0x38, Cli = 0x58, Sei = 0x78,
    Tya = 0x98, Clv = 0xB8, Cld = 0xD8, Sed = 0xF8,
    // _A
    Txa = 0x8A, Txs = 0x9A, Tax = 0xAA, Tsx = 0xBa,
    Dex = 0xCA, Nop = 0xEA,
    // Others
    Brk = 0x00, Jsr = 0x20, Rti = 0x40, Rts = 0x60,
}
#[derive(Debug, Copy, Clone)]
pub enum Op {
    Special(u8),
    Standard(u8),
    Branch(StatFlag, bool),
}
#[derive(Debug, Copy, Clone)]
pub enum Amode {
    Nothing, Accum, Rela, Immed,
    Zp, Zpx, Zpy, Idrx, Idry, Abs, Absx, Absy, Error,
}
#[allow(dead_code)]
pub enum Addr {
    Reset = 0xfffa,
    NMI   = 0xfffc,
    Break = 0xfffe,
}
pub struct Rom {
    pub rom: Vec<u8>,
    pub offset: u16,
}
impl ::std::ops::Index<u16> for Rom {
    type Output = u8;
    fn index(&self, index: u16) -> &u8 {
        &self.rom[(index - self.offset) as usize]
    }
}
impl ::std::ops::IndexMut<u16> for Rom {
    fn index_mut(&mut self, index: u16) -> &mut u8 {
        assert!(index >= self.offset, "Index out of range");
        &mut self.rom[(index - self.offset) as usize]
    }
}
impl Rom {
    fn get16(&self, index: u16) -> u16 {
        self[index] as u16 | (self[index+1] as u16) << 8
    }
}
#[derive(Copy, Clone, Debug)]
pub enum StatFlag {
    C = 1 << 0,
    Z = 1 << 1,
    I = 1 << 2,
    D = 1 << 3,
    B = 1 << 4,
    V = 1 << 6,
    S = 1 << 7,
}
