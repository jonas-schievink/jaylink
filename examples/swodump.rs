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
    frequency: u32,

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
    eprintln!("Max. SWO Speed: {}", speeds.max_speed());
    eprintln!(
        "Configuring at {} Hz ({} Bytes/sec) with a {} Byte buffer",
        opts.frequency,
        opts.frequency / 8,
        opts.probe_buf,
    );
    eprintln!("-----------------------------------------");

    probe.select_interface(Interface::Swd)?;
    probe.swo_stop()?;
    let stream = probe.swo_start(SwoMode::Uart, opts.frequency, opts.probe_buf)?;
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
