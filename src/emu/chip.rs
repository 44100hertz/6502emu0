// Here the "chip" is just the execution part of the processor.
// Bugs:
// - arithmetic has not been tested
// - missing undocumented (cc = 11) simultanious instructions
// - RMW instructions do not write memory twice like the real thing (WONTFIX)

use super::{Addr, Amode, StatFlag, Op, Rom};
use super::decode::decode;

use std::mem::transmute;

mod reg {
    // read: get an 8-bit char
    // write: write an 8-bit char
    pub const IO: u16 = 0x6000;
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

impl Chip {
    pub fn new(rom: Rom) -> Self {
        Chip {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xff,
            status: 0,
            pc: Addr::Reset as _,
            rom: rom,
            mem: vec![],
        }
    }
    pub fn run(&mut self) {
        self.mem.resize(0x10000 - self.rom.offset as usize, 0);
        self.pc = self.rom.get16(self.pc);
        while !self.get_flag(StatFlag::B) {
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
        use super::SpecialOp::*;
        use super::StandardOp::*;

        macro_rules! load{() => {{
            let tmp = self.load(mode, arg);
            tmp
        }}}
        macro_rules! store{($e:expr) => {{
            let v = $e;
            self.store(mode, arg, v)
        }}}
        macro_rules! update_flags{($e:expr) => {{
            let v = $e;
            self.update_flags(v)
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
            update_flags!($param);
        }}}
        macro_rules! transfer{($src:expr, $dest:expr) => {{
            $dest = $src;
            update_flags!($dest);
        }}}
        macro_rules! add{($param:expr) => {{
            let val = $param as u16;
            let sum = val + self.a as u16 +
                if self.get_flag(StatFlag::C) {1} else {0};
            self.set_flag(StatFlag::C, sum > 0xff);
            let a = self.a;
            self.set_flag(StatFlag::V,
                          val as u8 & 0x80 == a & 0x80 &&
                          val & 0x80 != sum & 0x80);
            self.a = sum as u8;
            self.update_flags(a);
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
                Lsr => store!(load!().wrapping_shr(1)),
                Asl => store!(load!().wrapping_shl(1) |
                              if self.get_flag(StatFlag::C) {1} else {0}),
                Ror => {
                    let (val, carry) = load!().overflowing_shr(1);
                    store!(val | if self.get_flag(StatFlag::C) {0x80} else {0});
                    self.set_flag(StatFlag::C, carry);
                }
                Rol => {
                    let (val, carry) = load!().overflowing_shl(1);
                    store!(val | if self.get_flag(StatFlag::C) {1} else {0});
                    self.set_flag(StatFlag::C, carry);
                }
                // other arithmetic
                Adc => add!(load!()),
                Sbc => add!(load!() ^ 0xff),
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
                Brk => self.set_flag(StatFlag::B, true),
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
                Txa => transfer!(self.x, self.a),
                Tax => transfer!(self.a, self.x),
                Tya => transfer!(self.y, self.a),
                Tay => transfer!(self.a, self.y),
                Txs => self.sp = self.x,
                Tsx => transfer!(self.sp, self.x),
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
        self.update_flags(ret);
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

    fn get_addr(&mut self, mode: Amode, pos: u16) -> u16 {
        use super::Amode::*;
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

    fn read_mem(&self, pos: u16) -> u8 {
        //println!("read memory at: {:x}", pos);
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos],
            reg::IO => {
                use std::io::{Read, stdin};
                let mut tmp = [0u8; 1];
                stdin().read(&mut tmp).expect("failed to read stdin");
                tmp[0]
            }
            _ => self.mem[pos as usize],
        }
    }

    fn write_mem(&mut self, pos: u16, v: u8) {
        //println!("write memory at: {:x}", pos)
        match pos {
            _ if pos >= self.rom.offset => self.rom[pos] = v,
            reg::IO => print!("{}", v as char),
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

    fn update_flags(&mut self, val: u8) {
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
