mod emu;

fn main() {
    use std::fs::File;
    use std::io::Read;

    let mut bin = vec![];
    File::open("test.bin").unwrap().
        read_to_end(&mut bin).unwrap();

    let rom = emu::Rom {
        rom: bin,
        offset: 0x8000,
    };

    emu::run(rom, 0.01);
}
