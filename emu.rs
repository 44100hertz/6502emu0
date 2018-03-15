fn main() {
    use std::fs::File;
    use std::io::Read;

    let mut bin = vec![];
    File::open("test.bin").unwrap().
        read_to_end(&mut bin).unwrap();

    let rom = asm::Rom {
        rom: bin,
        offset: 0x8000,
    };

    asm::Chip::run(rom);
}

mod asm {
    enum Addr {
        Reset = 0xfffa,
        NMI   = 0xfffc,
        Break = 0xfffe,
    }
    pub struct Rom {
        pub rom: Vec<u8>,
        pub offset: u16,
    }
    pub struct Chip {
        a: u8,
        pc: u16,
        rom: Rom,
    }
    impl Rom {
        fn get8(&self, index: u16) -> u8 {
            assert!(index >= self.offset, "Index out of range");
            self.rom[(index - self.offset) as usize]
        }
        fn get16(&self, index: u16) -> u16 {
            self.get8(index) as u16 | (self.get8(index+1) as u16) << 8
        }
    }

    // Opcodes provided with middle (argument) bits ___XXX__ set to 0
    // http://www.llx.com/~nparker/a2/opcodes.html
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
    // Other opcodes best to list invidiually
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
    }
    #[derive(Debug)]
    enum Amode {
        Nothing, Accum, Immed, Zp, Zpx, Zpy, Izpx, Izpy, Abs, Absx, Absy, Error,
    }
    fn decode(code: u8) -> (u8, Op, Amode) {
        use self::Amode::*;

        if code & 0x10 == 0x10 || // branch
            code & 0x9F == 0x0 ||   // brk, jsr, rti, rts
            code & 0x0f == 0x8 ||   // _8
            code & 0x0f == 0xA && code & 0xf0 >= 0x80 // _A
        {
            let (width, mode) = if code == SpecialOp::Jsr as u8 {
                (2, Abs)
            } else {
                (0, Nothing)
            };
            return (width, self::Op::Special(code), mode);
        }

        let op = code & 0b11100011;
        let addr_bits = code >> 2 & 7;
        let code_bits = code & 3;

        let (width, mode) = match addr_bits {
            0 => match code_bits {
                1 => (1, Izpx),
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
                1 => (1, Izpy),
                _ => (0, Error),
            }
            5 => (1, Zpx),
            6 => match code_bits {
                1 => (2, Absy),
                _ => (0, Error),
            }
            7 => (2, Absx),
            _ => unreachable!(),
        };

        (width, self::Op::Standard(op), mode)
    }
    impl Chip {
        pub fn run(rom: Rom) {
            Chip {
                a: 0,
                pc: Addr::Reset as _,
                rom: rom,
            }.real_run();
        }

        fn real_run(&mut self) {
            self.pc = self.rom.get16(self.pc);
            for _ in 1..10 {
                let (width, opcode, mode) = decode(self.rom.get8(self.pc));
                self.pc += 1;
                let v = match width {
                    0 => 0,
                    1 => self.rom.get8(self.pc) as u16,
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
            use std::mem::transmute;
            match code {
                Op::Standard(op) => match unsafe { transmute(op) } {
                    Sta => { let a = self.a; self.store(mode, arg, a) },
                    Lda => { self.a = self.load(mode, arg) },
                    _ => {},
                },
                Op::Special(op) => match unsafe { transmute(op) } {
                    Brk => self.pc = self.rom.get16(Addr::Break as u16),
                    _ => {},
                },
            }
        }

        fn load(&self, mode: Amode, pos: u16) -> u8 {
            use self::Amode::*;
            match mode {
                Immed => pos as u8,
                _ => unimplemented!(),
            }
        }

        fn store(&mut self, mode: Amode, pos: u16, v: u8) {
            use self::Amode::*;
            match mode {
                Zp | Abs => self.write_mem(pos, v),
                _ => unimplemented!(),
            }
        }

        fn write_mem(&mut self, pos: u16, v: u8) {
            if pos == 0x6000 {
                println!("{}", v as char);
            }
        }
    }
}
