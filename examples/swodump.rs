//! Simple SWO capture demo that prints decoded ITM packets to stdout.
//!
//! Note that this does not reconfigure the target MCU to enable the ITM ports, set the speed, or do
//! anything else that might be required before the MCU starts outputting SWO data. It is expected
//! that the firmware will perform these steps.
//!
//! More advanced tooling would do this automatically, but the mechanism involved is
//! vendor-specific, so `jaylink` does not know how to do this.

use jaylink::Interface;
use jaylink::{JayLink, SwoMode};
use std::error::Error;
use std::io::Write;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opts {
    /// Serial number of the probe to connect to.
    #[structopt(long = "serial")]
    serial: Option<String>,

    /// Frequency/Baudrate to sample SWO at.
    #[structopt(long = "freq", short = "f")]
    frequency: Option<u32>,

    /// Size of on-probe buffer to allocate.
    #[structopt(long = "probe-buf", default_value = "1024")]
    probe_buf: u32,
}

fn main() {
    env_logger::init();

    let opts = Opts::from_args();
    if let Err(e) = run(opts) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

fn run(opts: Opts) -> Result<(), Box<dyn Error>> {
    let mut probe = JayLink::open_by_serial(opts.serial.as_deref())?;

    let speeds = probe.read_swo_speeds(SwoMode::Uart)?;
    eprintln!("Max. SWO Speed: {} Hz", speeds.max_speed_hz());

    let frequency = opts.frequency.unwrap_or(speeds.max_speed_hz());
    eprintln!(
        "Configuring at {} Hz ({} Bytes/sec) with a {} Byte buffer",
        frequency,
        frequency / 8,
        opts.probe_buf,
    );
    eprintln!("-----------------------------------------");

    probe.select_interface(Interface::Swd)?;
    probe.swo_stop()?;
    let stream = probe.swo_start(SwoMode::Uart, frequency, opts.probe_buf)?;
    let mut stream = itm::Decoder::new(stream, false);
    let out = std::io::stdout();
    let mut out = out.lock();

    loop {
        match stream.read_packet() {
            Ok(packet) => {
                if let itm::packet::Kind::Instrumentation(i) = packet.kind() {
                    out.write_all(i.payload())?;
                }
            }
            Err(e) => {
                eprintln!("ITM decode error: {}", e);
            }
        }
    }
}
