use std::cmp;

use jaylink::{CommunicationSpeed, Interface, JayLink};
use structopt::StructOpt;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(StructOpt)]
struct Opts {
    /// Serial number of the probe to connect to.
    #[structopt(long = "serial")]
    serial: Option<String>,

    /// Communication speed in kHz.
    #[structopt(long = "speed", default_value = "200")]
    speed: u16,
}

fn main() {
    env_logger::init();

    let opts = Opts::from_args();
    if let Err(e) = run(opts) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

fn run(opts: Opts) -> Result<()> {
    let mut probe = JayLink::open_by_serial(opts.serial.as_deref())?;

    probe.select_interface(Interface::Pic32Icsp)?;

    // Limit speed so invalid 0xffff doesn't appear
    let khz = cmp::min(opts.speed, 0xfffe);
    probe.set_speed(CommunicationSpeed::khz(khz).unwrap())?;

    // "PIC32 Flash- and Programming-Specification"
    // 7.0 "Entering 2-Wire Enhanced ICSP Mode"

    // 4D 43 48 50

    println!("intf before: {}", probe.read_interface()?);
    let bits = probe
        .jtag_io(vec![true, false], vec![true, false])?
        .collect::<Vec<_>>();
    println!("intf after: {}", probe.read_interface()?);

    println!("Done: {:?}", bits);

    Ok(())
}
