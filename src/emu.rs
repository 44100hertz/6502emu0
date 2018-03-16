use std::mem::transmute;

// Variable opcodes with middle (argument) bits ___XXX__ set to 0
// http://www.llx.com/~nparker/a2/opcodes.html
#[allow(dead_code)]
enum StandardOp {
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
enum SpecialOp {
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
enum Op {
    Special(u8),
    Standard(u8),
    Branch(StatFlag, bool),
}
#[derive(Debug)]
enum Amode {
    Nothing, Accum, Rela, Immed,
    Zp, Zpx, Zpy, Idrx, Idry, Abs, Absx, Absy, Error,
}
enum Addr {
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
        assert!(index >= self.offset, "Index out of range");
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
pub struct Chip {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    status: u8,
    rom: Rom,
    mem: Vec<u8>,
}
#[derive(Copy, Clone, Debug)]
enum StatFlag {
    C = 1 << 0,
    Z = 1 << 1,
    I = 1 << 2,
    D = 1 << 3,
    B = 1 << 4,
    V = 1 << 6,
    S = 1 << 7,
}
impl Chip {
    pub fn run(rom: Rom) {
        Chip {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xff,
            status: 0,
            pc: Addr::Reset as _,
            rom: rom,
            mem: vec![],
        }.real_run();
    }

    fn real_run(&mut self) {
        self.mem.resize(0x10000 - self.rom.offset as usize, 0);
        self.pc = self.rom.get16(self.pc);
        for _ in 1..100 {
            let (width, opcode, mode) = decode(self.rom[self.pc]);
            self.pc += 1;
            let v = match width {
                0 => 0,
                1 => self.rom[self.pc] as u16,
                2 => self.rom.get16(self.pc),
                _ => unreachable!(),
            };
            self.exec(opcode, mode, v);
            self.pc += width as u16;
        }
    }

    fn exec(&mut self, code: Op, mode: Amode, arg: u16) {
        use self::SpecialOp::*;
        use self::StandardOp::*;
        match code {
            Op::Branch(flag, cmp) => {
                if (self.status & flag as u8 == 0) == cmp {
                    self.pc = (self.pc as i16 + (arg as i8) as i16) as u16;
                }
            }
            Op::Standard(op) => match unsafe { transmute(op) } {
                Sta => { let a = self.a; self.store(mode, arg, a) }
                Lda => { self.a = self.load(mode, arg) }
                Ldx => { self.x = self.load(mode, arg) }
                Ldy => { self.y = self.load(mode, arg) }
                _ => {},
            }
            Op::Special(op) => match unsafe { transmute(op) } {
                Brk => { self.pc = self.rom.get16(Addr::Break as u16) }
                Inx => {
                    let x = self.x.wrapping_add(1);
                    self.x = x;
                    self.update_zs(x);
                }
                Iny => {
                    let y = self.y.wrapping_add(1);
                    self.y = y;
                    self.update_zs(y);
                }
                _ => {}
            }
        }
    }

    fn get_addr(&mut self, mode: Amode, pos: u16) -> u16 {
        use self::Amode::*;
        //        println!("load: {:?} at {:x}", mode, pos);
        match mode {
            Zp | Abs => pos,
            Zpx => pos << 8 | self.x as u16,
            Zpy => pos << 8 | self.y as u16,
            Absx => pos.wrapping_add(self.x as u16),
            Absy => pos.wrapping_add(self.y as u16),
            Idrx => {
                let pos = pos + self.x as u16;
                self.read_mem(pos) as u16 |
                (self.read_mem(pos+1) as u16) << 8
            },
            Idry => {
                self.read_mem(pos).wrapping_add(self.x) as u16 |
                (self.read_mem(pos+1) as u16) << 8
            },
            Accum | Immed | Nothing | Rela => unreachable!(),
            Error => panic!("Invalid opcode"),
        }
    }
    fn load(&mut self, mode: Amode, pos: u16) -> u8 {
        let ret = match mode {
            Amode::Immed => pos as u8,
            Amode::Accum => self.a,
            _ => {
                let addr = self.get_addr(mode, pos);
                self.read_mem(addr)
            }
        };
        self.update_zs(ret);
        ret
    }

    fn store(&mut self, mode: Amode, pos: u16, v: u8) {
        match mode {
            Amode::Immed => panic!("Attempt to store immediate"),
            Amode::Accum => self.a = v,
            _ => {
                let addr = self.get_addr(mode, pos);
                self.write_mem(addr, v);
            }
        }
    }

    fn read_mem(&self, pos: u16) -> u8 {
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos],
            _ => self.mem[pos as usize],
        }
    }

    fn write_mem(&mut self, pos: u16, v: u8) {
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos] = v,
            0x6000 => print!("{}", v as char),
            _ => self.mem[pos as usize] = v,
        }
    }

    fn push8(&mut self, data: u8) {
        self.mem[0x100 + self.sp as usize] = data;
        self.sp -= 1;
        if self.sp == 0xff {
            eprintln!("Warning: stack overflow");
        }
    }

    fn push16(&mut self, data: u16) {
        self.push8((data >> 8) as u8);
        self.push8(data as u8);
    }

    fn update_zs(&mut self, val: u8) {
        self.set_flag(StatFlag::Z, val == 0);
        self.set_flag(StatFlag::S, val & 0x80 != 0);
    }

    fn set_flag(&mut self, flag: StatFlag, value: bool) {
        let bit = if value { flag as u8 } else { 0 };
        self.status = self.status & (0xff ^ flag as u8) | bit;
    }
}

fn decode(code: u8) -> (u8, Op, Amode) {
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
            code >> 5 & 1 == 0, // true means test against 0
        );
        return (1, op, Rela);
    }

    // special codes
    if code & 0x9F == 0x0 ||
        code & 0x0f == 0x8 ||
        code & 0x0f == 0xA && code & 0xf0 >= 0x80
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
