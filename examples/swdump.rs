//! Dumps target information via SWD.

use bitvec::{cursor::LittleEndian, vec::BitVec};
use jaylink::*;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opts {
    /// Serial number of the probe to connect to.
    #[structopt(long = "serial")]
    serial: Option<String>,
}

fn main() {
    let opts = Opts::from_args();
    if let Err(e) = run(opts) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

fn run(opts: Opts) -> Result<()> {
    let mut probe = JayLink::open_by_serial(opts.serial.as_ref().map(|s| &**s))?;

    probe.set_speed(CommunicationSpeed::khz(10).unwrap())?;

    let mut dir = BitVec::<LittleEndian, u8>::new();
    let mut swdio = BitVec::<LittleEndian, u8>::new();

    // Read DP address 0b0000 = DPIDR
    swdio.push(true); // start bit
    swdio.push(false); // false = DP, true = AP
    swdio.push(true); // false = Write, true = Read
    swdio.push(false); // address
    swdio.push(false); // address
    swdio.push(false); // even parity
    swdio.push(false); // stop bit
    swdio.push(true); // park bit
    dir.resize(dir.len() + 8, false); // write

    swdio.push(false); // ACK
    swdio.push(false); // ACK
    swdio.push(false); // ACK

    for _ in 0..32 {
        swdio.push(false); // 32 bits of read data
    }

    dir.resize(dir.len() + 3 + 32, false); // read

    println!("sending raw swdio: {:?}", swdio);
    bindump(swdio.as_slice());
    probe.swd_io(&dir, &mut swdio)?;

    println!("received raw swdio: {:?}", swdio);
    bindump(swdio.as_slice());

    unimplemented!();
}

fn bindump(bytes: &[u8]) {
    let dump = bytes
        .iter()
        .map(|byte| format!("{:08b}", byte))
        .collect::<Vec<_>>()
        .join(", ");
    println!("{}", dump);
}
