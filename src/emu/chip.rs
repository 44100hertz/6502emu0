// Here the "chip" is just the execution part of the processor.
// Differences with real hardware:
// - missing undocumented (cc = 11) simultanious instructions
// - crossing a page is not slower on branch or indexing
// - instruction delays untested (untestable?), maybe be inaccurate
// - BRK just closes the emulator (intentionally)
// - RMW instructions do not write memory twice (WONTFIX)
// Bugs:
// - After 2^32 cycles, speed becomes unbounded. (about an hour at 1mhz)

use super::{Addr, Amode, StatFlag, Op, Rom};
use super::decode::decode;

use std::mem::transmute; // for enums
use std::time::{Instant, Duration};

mod reg {
    pub const IO: u16      = 0x6000; // read: get char; write: put char
    pub const PUTNUM: u16  = 0x6001; // writes hex value
    pub const PUTDEC: u16  = 0x6002; // writes decimal value
    pub const PUTPAGE: u16 = 0x6003; // writes a 256 byte mem region
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
    start_time: Instant,
    cpu_time: Duration,
    cycle_len: Duration,
    lag_count: u32,
}

impl Chip {
    pub fn new(rom: Rom, mhz: f64) -> Self {
        Chip {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xff,
            status: 0,
            pc: Addr::Reset as _,
            rom: rom,
            mem: vec![],
            start_time: Instant::now(),
            cycle_len: Duration::new(0, (1000.0 / mhz) as u32),
            cpu_time: Duration::new(0, 0),
            lag_count: 0,
        }
    }
    pub fn run(&mut self) {
        self.mem.resize(0x10000 - self.rom.offset as usize, 0);
        self.pc = self.rom.get16(self.pc);
        while !self.get_flag(StatFlag::B) {
//            println!("pc   a  x  y  SV_BDIZC");
//            println!("{:04x} {:02x} {:02x} {:02x} {:08b}", self.pc, self.a, self.x, self.y, self.status);
            let (opcode, mode) = decode(self.read_mem(self.pc));
            let width = mode.width();
            let arg_pos = self.pc + 1;
            self.pc += width as u16 + 1;
            let v = match width {
                0 => 0,
                1 => self.rom[arg_pos] as u16,
                2 => self.rom.get16(arg_pos),
                _ => unreachable!(),
            };
            self.exec(opcode, mode, v);
            self.cycle(2);
        }
    }

    fn exec(&mut self, code: Op, mode: Amode, arg: u16) {
        use super::SpecialOp::*;
        use super::StandardOp::*;

        macro_rules! load{() => {{
            let tmp = self.load(mode, arg);
            tmp
        }}}
        macro_rules! store{($e:expr) => {{
            let v = $e;
            self.store(mode, arg, v);
            v
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
            self.cycle(2);
            $param = $param.wrapping_add($offset);
            update_flags!($param);
        }}}
        macro_rules! transfer{($src:expr, $dest:expr) => {{
            $dest = $src;
            update_flags!($dest);
        }}}
        macro_rules! add{($param:expr) => {{
            let a = self.a;
            let val = $param as u16;
            let carry_in = if self.get_flag(StatFlag::C) { 1 } else { 0 };
            let sum = val + self.a as u16 + carry_in;
            self.set_flag(StatFlag::C, sum > 0xff);
            self.set_flag(StatFlag::V,
                          val as u8 & 0x80 == a & 0x80 && // input signs same
                          val & 0x80 != sum & 0x80); // result has other sign
            self.a = sum as u8;
            update_flags!(self.a);
        }}}
        macro_rules! shift{($rotation:expr, $is_left:expr) => {{
            self.cycle(2);
            let input = load!();
            let (shifted, in_bit, out_bit) = match $is_left {
                true =>  (input << 1, 1, 0x80),
                false => (input >> 1, 0x80, 1),
            };
            let carry_in = $rotation && self.get_flag(StatFlag::C);
            let output = shifted | if carry_in {in_bit} else {0};
            self.set_flag(StatFlag::C, input & out_bit != 0);
            update_flags!(store!(output));
        }}}
        //println!("exec: {:?} {:?} {:x}", code, mode, arg);
        match code {
            Op::Branch(flag, cmp) => {
                self.cycle(2);
                if self.get_flag(flag) == cmp {
                    self.cycle(1);
                    self.pc = (self.pc as i16 + (arg as i8) as i16) as u16;
                }
            }
            Op::Standard(op) => match unsafe { transmute(op) } {
                // load/store
                Lda => self.a = load!(),
                Ldx => self.x = load!(),
                Ldy => self.y = load!(),
                Sta => { store!(self.a); },
                Stx => { store!(self.x); },
                Sty => { store!(self.y); },
                // read modify write
                Inc => update_flags!(store!(load!().wrapping_add(1))),
                Dec => update_flags!(store!(load!().wrapping_sub(1))),
                Lsr => shift!(false, false),
                Asl => shift!(false, true),
                Ror => shift!(true,  false),
                Rol => shift!(true,  true),
                // other arithmetic
                Adc => add!(load!()),
                Sbc => add!(load!() ^ 0xff),
                Ora => { self.a |= load!(); update_flags!(self.a) },
                And => { self.a &= load!(); update_flags!(self.a) },
                Eor => { self.a ^= load!(); update_flags!(self.a) },
                // control flow
                Jmp => { self.cycle(1); self.pc = arg },
                Jma => {
                    self.cycle(3);
                    self.pc = self.read_mem(arg) as u16
                        | (self.read_mem((arg + 1) & 0xff) as u16 >> 8)
                },
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
                    self.cycle(2);
                    let pc = self.pc - 1;
                    self.push16(pc);
                    self.pc = arg;
                }
                Rts => { self.cycle(2); self.pc = self.pop16() + 1 },
                Rti => {
                    self.cycle(1);
                    self.status = self.pop();
                    self.pc = self.pop16()
                },
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
                Plp => { self.cycle(1); self.status = self.pop() },
                Pha => push!(self.a),
                Pla => { self.cycle(1); self.a = self.pop() },
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
            Zp => { self.cycle(1); pos },
            Abs => { self.cycle(2); pos },
            Zpx => { self.cycle(2); pos << 8 | self.x as u16 },
            Zpy => { self.cycle(2); pos << 8 | self.y as u16 },
            Absx => { self.cycle(2); pos.wrapping_add(self.x as u16) },
            Absy => { self.cycle(2); pos.wrapping_add(self.y as u16) },
            Idrx => {
                self.cycle(4);
                let pos = pos + self.x as u16;
                self.read_mem(pos) as u16 |
                ((self.read_mem(pos+1) as u16) << 8)
            },
            Idry => {
                self.cycle(4);
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
            reg::PUTNUM => print!("{:02x} ", v),
            reg::PUTDEC => print!("{:03} ", v),
            reg::PUTPAGE => {
                const PAGE_W: u16 = 32;
                print!("     ");
                for i in 0..PAGE_W { print!("{:02x}", i); }
                println!();
                for i in 0..256 {
                    let offset = i + v as u16 * 256;
                    if i % PAGE_W == 0 { print!("{:04x} ", offset); }
                    print!("{:02x}", self.read_mem(offset));
                    if i % PAGE_W == PAGE_W-1 { println!(); }
                }
            }
            _ => self.mem[pos as usize] = v,
        }
    }

    fn pop(&mut self) -> u8 {
        self.cycle(1);
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
        self.cycle(1);
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
        self.set_flag(StatFlag::S, (val as i8) < 0);
    }
    fn get_flag(&mut self, flag: StatFlag) -> bool {
        self.status & flag as u8 != 0
    }
    fn set_flag(&mut self, flag: StatFlag, value: bool) {
        let bit = if value { flag as u8 } else { 0 };
        self.status = self.status & (0xff ^ flag as u8) | bit;
    }

    fn cycle(&mut self, cycles: u32) {
        self.cpu_time += self.cycle_len * cycles;
        let real_time = self.start_time.elapsed();
        if let Some(wait_time) = self.cpu_time.checked_sub(real_time) {
            self.lag_count = 0;
            ::std::thread::sleep(wait_time);
        } else {
            self.lag_count += cycles;
            if self.lag_count > 1000 {
                eprintln!("!! lag !!");
                self.lag_count = 0;
            }
        }
    }
}
