//! Uses the functions for controlling individual pin states to toggle them periodically.

use jaylink::*;
use std::thread::sleep;
use std::time::Duration;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opts {
    /// Serial number of the probe to connect to.
    #[structopt(long = "serial")]
    serial: Option<String>,
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

    // Enable power to enable testing all blinky pins without an ext. supply.
    // Ignore errors since probes may not support this.
    probe.set_kickstart_power(true).ok();

    loop {
        probe.set_tms(true)?;
        probe.set_tdi(true)?;
        probe.set_reset(true)?;
        probe.set_trst(true)?;
        println!("on  {} V", probe.read_target_voltage()? as f32 / 1000.0);
        sleep(Duration::from_millis(500));
        probe.set_tms(false)?;
        probe.set_tdi(false)?;
        probe.set_reset(false)?;
        probe.set_trst(false)?;
        println!("off {} V", probe.read_target_voltage()? as f32 / 1000.0);
        sleep(Duration::from_millis(500));
    }
}
