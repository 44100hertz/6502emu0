use std::mem::transmute;

// Variable opcodes with middle (argument) bits ___XXX__ set to 0
// http://www.llx.com/~nparker/a2/opcodes.html
#[allow(dead_code)]
#[repr(u8)]
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
#[repr(u8)]
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
#[derive(Debug, Copy, Clone)]
enum Op {
    Special(u8),
    Standard(u8),
    Branch(StatFlag, bool),
}
#[derive(Debug, Copy, Clone)]
enum Amode {
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
pub struct Chip {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    status: u8,
    rom: Rom,
    mem: Vec<u8>,
    running: bool,
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
            running: true,
        }.real_run();
    }

    fn real_run(&mut self) {
        self.mem.resize(0x10000 - self.rom.offset as usize, 0);
        self.pc = self.rom.get16(self.pc);
        while self.running {
//println!("pc: {:x}, a: {:x}, x: {:x}, y: {:x}, status: {:x}", self.pc, self.a, self.x, self.y, self.status);
            let (width, opcode, mode) = decode(self.rom[self.pc]);
            let arg_pos = self.pc + 1;
            self.pc += width as u16 + 1;
            let v = match width {
                0 => 0,
                1 => self.rom[arg_pos] as u16,
                2 => self.rom.get16(arg_pos),
                _ => unreachable!(),
            };
            self.exec(opcode, mode, v);
        }
    }

    fn exec(&mut self, code: Op, mode: Amode, arg: u16) {
//println!("exec: {:?} {:?} {:x}", code, mode, arg);
        use self::SpecialOp::*;
        use self::StandardOp::*;

        macro_rules! load{() => {{
            let tmp = self.load(mode, arg);
            tmp
        }}}
        macro_rules! store{($e:expr) => {{
            let v = $e;
            self.store(mode, arg, v)
        }}}
        macro_rules! update_zs{($e:expr) => {{
            let v = $e;
            self.update_zs(v)
        }}}
        macro_rules! push{($e:expr) => {{
            let v = $e;
            self.push(v)
        }}}
        macro_rules! cmp{($e:expr) => {{
            let v = $e;
            let loaded = load!();
            self.set_flag(StatFlag::Z, v == loaded);
        }}}
        macro_rules! inc{($param:expr, $offset:expr) => {{
            $param = $param.wrapping_add($offset);
            update_zs!($param);
        }}}
        match code {
            Op::Branch(flag, cmp) => {
                if self.get_flag(flag) == cmp {
                    self.pc = (self.pc as i16 + (arg as i8) as i16) as u16;
                }
            }
            Op::Standard(op) => match unsafe { transmute(op) } {
                // load/store
                Lda => self.a = load!(),
                Ldx => self.x = load!(),
                Ldy => self.y = load!(),
                Sta => store!(self.a),
                Stx => store!(self.x),
                Sty => store!(self.y),
                // read modify write
                Dec => store!(load!().wrapping_sub(1)),
                Inc => store!(load!().wrapping_add(1)),
                Lsr => unimplemented!(),
                Asl => unimplemented!(),
                Ror => unimplemented!(),
                Rol => unimplemented!(),
                // other arithmetic
                Adc => unimplemented!(),
                Sbc => unimplemented!(),
                Ora => self.a |= load!(),
                And => self.a &= load!(),
                Eor => self.a ^= load!(),
                // control flow
                Jmp => self.pc = arg,
                Jma => self.pc = self.read_mem(arg) as u16 |
                (self.read_mem((arg + 1) & 0xff) as u16 >> 8),
                Cmp => cmp!(self.a),
                Cpx => cmp!(self.x),
                Cpy => cmp!(self.y),
                // misc
                Bit => { load!(); },
            }
            Op::Special(op) => match unsafe { transmute(op) } {
                // control flow
                Brk => self.running = false,
                Jsr => {
                    let pc = self.pc - 1;
                    self.push16(pc);
                    self.pc = arg;
                }
                Rts => self.pc = self.pop16() + 1,
                Rti => {self.status = self.pop(); self.pc = self.pop16()},
                Inx => inc!(self.x, 1),
                Iny => inc!(self.y, 1),
                Dex => inc!(self.x, 255),
                Dey => inc!(self.y, 255),
                // misc
                Txa => self.a = self.x,
                Tax => self.x = self.a,
                Tya => self.a = self.y,
                Tay => self.y = self.a,
                Txs => self.sp = self.x,
                Tsx => self.x = self.sp,
                Clc => self.set_flag(StatFlag::C, false),
                Sec => self.set_flag(StatFlag::C, true),
                Cli => self.set_flag(StatFlag::I, false),
                Sei => self.set_flag(StatFlag::I, true),
                Cld => self.set_flag(StatFlag::D, false),
                Sed => self.set_flag(StatFlag::D, true),
                Clv => self.set_flag(StatFlag::V, false),
                Php => push!(self.status),
                Plp => self.status = self.pop(),
                Pha => push!(self.a),
                Pla => self.a = self.pop(),
                Nop => {},
            }
        }
    }

    fn get_addr(&mut self, mode: Amode, pos: u16) -> u16 {
        use self::Amode::*;
        match mode {
            Zp | Abs => pos,
            Zpx => pos << 8 | self.x as u16,
            Zpy => pos << 8 | self.y as u16,
            Absx => pos.wrapping_add(self.x as u16),
            Absy => pos.wrapping_add(self.y as u16),
            Idrx => {
                let pos = pos + self.x as u16;
                self.read_mem(pos) as u16 |
                ((self.read_mem(pos+1) as u16) << 8)
            },
            Idry => {
                let offset = self.read_mem(pos) as u16 | (self.read_mem(pos+1) as u16) << 8;
//println!("indirect offset: {:x}", offset);
                offset.wrapping_add(self.y as u16)
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
//println!("load: {:x} from {:?} at {:x}", ret, mode, pos);
        self.update_zs(ret);
        ret
    }

    fn store(&mut self, mode: Amode, pos: u16, v: u8) {
//println!("store: {:x} into {:?} at {:x}", v, mode, pos);
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
//println!("read memory at: {:x}", pos);
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos],
            _ => self.mem[pos as usize],
        }
    }

    fn write_mem(&mut self, pos: u16, v: u8) {
//println!("write memory at: {:x}", pos)
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos] = v,
            0x6000 => print!("{}", v as char),
            _ => self.mem[pos as usize] = v,
        }
    }

    fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let ret = self.mem[0x100 + self.sp as usize];
//println!("pop: {:x} at sp = {:x}", ret, self.sp);
        if self.sp == 0x00 {
            eprintln!("stack underflow");
        }
        ret
    }
    fn pop16(&mut self) -> u16 {
        self.pop() as u16 | (self.pop() as u16) << 8
    }
    fn push(&mut self, data: u8) {
//println!("push: {:x} at sp = {:x}", data, self.sp);
        self.mem[0x100 + self.sp as usize] = data;
        self.sp = self.sp.wrapping_sub(1);
        if self.sp == 0xff {
            eprintln!("stack overflow");
        }
    }
    fn push16(&mut self, data: u16) {
        self.push((data >> 8) as u8);
        self.push(data as u8);
    }

    fn update_zs(&mut self, val: u8) {
        self.set_flag(StatFlag::Z, val == 0);
        self.set_flag(StatFlag::S, val & 0x80 != 0);
    }

    fn get_flag(&mut self, flag: StatFlag) -> bool {
        self.status & flag as u8 != 0
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
