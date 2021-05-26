//! Dumps target information via SWD.
//!
//! # Notes on SWD and the J-Link implementation
//!
//! There is no "J-Link implementation". J-Links simply bit-bang the data you give them, and switch
//! the direction according to the direction bits. That's it.
//!
//! How SWD works is specified in the ARM® Debug Interface Architecture Specification (ADI). At the
//! time of writing, ADIv6 seems to be the most recent one.
//!
//! ADI is very misleading on when sampling and setup of the signal happens (eg. in the example
//! signal diagrams).
//!
//! The SWD implementation in most (hopefully all) microcontrollers is broken and does not have a
//! turnaround between the initial write/read packet and the ACK bits. To compensate, there seems to
//! be an *extra* turnaround cycle between ACK and the written data. Luckily, this isn't a problem
//! since the host's packet is sampled on the leading clock edge, while ACK is sampled on the
//! falling edge, so theoretically there is 0.5 to 1.5 cycles in which the driving device can
//! switch.
//!
//! This example assumes that the above "bug" (or misspecification?) is present.

use jaylink::{Interface, JayLink, SpeedConfig};
use log::trace;
use std::{cmp, fmt};
use structopt::StructOpt;

const IDLE_CYCLES_BEFORE_ACCESS: usize = 2;

#[derive(StructOpt)]
struct Opts {
    /// Serial number of the probe to connect to.
    #[structopt(long = "serial")]
    serial: Option<String>,

    /// Communication speed in kHz.
    #[structopt(long = "speed")]
    speed: Option<u16>,
}

fn main() {
    env_logger::init();

    let opts = Opts::from_args();
    if let Err(e) = run(opts) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

enum Port {
    Debug,
    #[allow(unused)] // Not used by this demo
    Access,
}

#[derive(Debug)]
enum SwdError {
    Probe(jaylink::Error),
    Fault,
    NoResponse,
    Parity,
}

impl From<jaylink::Error> for SwdError {
    fn from(e: jaylink::Error) -> Self {
        SwdError::Probe(e)
    }
}

impl fmt::Display for SwdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SwdError::Probe(e) => e.fmt(f),
            SwdError::Fault => f.write_str("target returned FAULT response"),
            SwdError::NoResponse => f.write_str("no response from target chip"),
            SwdError::Parity => f.write_str("SWD parity error"),
        }
    }
}

enum Ack {
    Ok,
    Wait,
    Fault,
}

fn parse_ack(sl: &[bool]) -> Option<Ack> {
    assert_eq!(sl.len(), 3);
    trace!("ACK: {:?}", sl);

    Some(match (sl[0], sl[1], sl[2]) {
        (true, false, false) => Ack::Ok,
        (false, true, false) => Ack::Wait,
        (false, false, true) => Ack::Fault,
        _ => return None,
    })
}

trait JayLinkExt {
    fn swj_seq(&mut self) -> jaylink::Result<()>;
    fn raw_read(&mut self, port: Port, a: u32) -> Result<u32, SwdError>;
    fn raw_write(&mut self, port: Port, a: u32, value: u32) -> Result<(), SwdError>;
}

impl JayLinkExt for JayLink {
    fn swj_seq(&mut self) -> jaylink::Result<()> {
        let mut dir = Vec::new();
        let mut swdio = Vec::new();

        swdio.resize(64, true);
        dir.resize(64, true);

        let mut seq = 0xE79E; // LSb first
        for _ in 0..16 {
            swdio.push(seq & 0b1 != 0);
            seq >>= 1;
        }

        dir.resize(dir.len() + 16, true);

        swdio.resize(swdio.len() + 64, true);
        dir.resize(dir.len() + 64, true);

        // idle cycles = host clocks interface with SWDIO low
        swdio.resize(swdio.len() + 10, false);
        dir.resize(dir.len() + 10, true);

        self.swd_io(dir, swdio)?;

        Ok(())
    }

    fn raw_read(&mut self, port: Port, a: u32) -> Result<u32, SwdError> {
        let mut dir = Vec::new();
        let mut swdio = Vec::new();

        swdio.resize(swdio.len() + IDLE_CYCLES_BEFORE_ACCESS, false);
        dir.resize(dir.len() + IDLE_CYCLES_BEFORE_ACCESS, true);

        let a2 = a & 0b0100 != 0;
        let a3 = a & 0b1000 != 0;

        swdio.push(true); // start bit
        swdio.push(match port {
            Port::Debug => false,
            Port::Access => true,
        });
        swdio.push(true); // false = Write, true = Read
        swdio.push(a2);
        swdio.push(a3);
        // even parity, subtracting `start` bit
        let even_parity = (swdio.iter().filter(|b| **b).count() - 1) % 2 != 0;
        swdio.push(even_parity);
        swdio.push(false); // stop bit
        swdio.push(true); // park bit
        dir.resize(dir.len() + 8, true); // write these bits

        swdio.push(false); // ACK
        swdio.push(false); // ACK
        swdio.push(false); // ACK

        // 32 bits of read data
        swdio.resize(swdio.len() + 32, false);
        swdio.push(false); // parity

        dir.resize(dir.len() + 3 + 33, false); // read

        loop {
            let mut response = self.swd_io(dir.iter().copied(), swdio.iter().copied())?;
            response.split_off(IDLE_CYCLES_BEFORE_ACCESS + 8);
            trace!("response: {:?}", response);
            let ack = response.split_off(3).collect::<Vec<_>>();
            let value = response.split_off(32).collect::<Vec<_>>();
            let parity = response.next().unwrap();

            // Wanna see some fucked up shit? https://forum.sparkfun.com/viewtopic.php?t=29205
            // This code assumes that the J-Link hardware will handle this somehow, which is
            // probably a wrong assumption. It does appear to insert an extra clock cycle for the
            // turnaround though.

            if let Some(ack) = parse_ack(&ack) {
                match ack {
                    Ack::Ok => {
                        // Read 32-bit value and parity bit
                        let value = value
                            .iter()
                            .fold(0u32, |accum, bit| (accum >> 1) | (u32::from(*bit) << 31));
                        trace!("value=0x{:08X}, parity={:?}", value, parity);

                        let expected_parity = value.count_ones() % 2 != 0;
                        if expected_parity == parity {
                            return Ok(value);
                        } else {
                            return Err(SwdError::Parity);
                        }
                    }
                    Ack::Wait => {
                        trace!("WAIT - retrying read access");
                        continue;
                    }
                    Ack::Fault => {
                        return Err(SwdError::Fault);
                    }
                }
            }

            return Err(SwdError::NoResponse);
        }
    }

    fn raw_write(&mut self, port: Port, a: u32, value: u32) -> Result<(), SwdError> {
        let mut dir = Vec::new();
        let mut swdio = Vec::new();

        swdio.resize(swdio.len() + IDLE_CYCLES_BEFORE_ACCESS, false);
        dir.resize(dir.len() + IDLE_CYCLES_BEFORE_ACCESS, true);

        let a2 = a & 0b0100 != 0;
        let a3 = a & 0b1000 != 0;

        swdio.push(true); // start bit
        swdio.push(match port {
            Port::Debug => false,
            Port::Access => true,
        });
        swdio.push(false); // false = Write, true = Read
        swdio.push(a2);
        swdio.push(a3);
        // even parity, subtracting `start` bit
        let even_parity = (swdio.iter().filter(|b| **b).count() - 1) % 2 != 0;
        swdio.push(even_parity);
        swdio.push(false); // stop bit
        swdio.push(true); // park bit
        dir.resize(dir.len() + 8, true); // write these bits

        swdio.push(false); // ACK
        swdio.push(false); // ACK
        swdio.push(false); // ACK
        dir.resize(dir.len() + 3, false); // read ACK

        // read 2 turnaround cycles (see module comment)
        swdio.push(false);
        swdio.push(false);
        dir.resize(dir.len() + 2, false);

        // The value to write
        {
            let mut value = value;
            for _ in 0..32 {
                swdio.push(value & 1 != 0);
                value >>= 1;
            }
            swdio.push(value.count_ones() % 2 != 0); // parity
            dir.resize(dir.len() + 33, true);
        }

        loop {
            let mut response = self.swd_io(dir.iter().copied(), swdio.iter().copied())?;
            response.split_off(IDLE_CYCLES_BEFORE_ACCESS + 8);
            trace!("response: {:?}", response);
            let ack = response.split_off(3).collect::<Vec<_>>();

            // Wanna see some fucked up shit? https://forum.sparkfun.com/viewtopic.php?t=29205
            // This code assumes that the J-Link hardware will handle this somehow, which is
            // probably a wrong assumption. It does appear to insert an extra clock cycle for the
            // turnaround though.

            if let Some(ack) = parse_ack(&ack) {
                match ack {
                    Ack::Ok => {
                        // NOTE: This doesn't mean the write succeeded.
                        return Ok(());
                    }
                    Ack::Wait => {
                        trace!("WAIT - retrying write access");
                        continue;
                    }
                    Ack::Fault => {
                        return Err(SwdError::Fault);
                    }
                }
            }

            return Err(SwdError::NoResponse);
        }
    }
}

fn run(opts: Opts) -> Result<(), SwdError> {
    let mut probe = JayLink::open_by_serial(opts.serial.as_deref())?;

    probe.select_interface(Interface::Swd)?;

    // Limit speed so invalid 0xffff doesn't appear
    let khz = cmp::min(opts.speed.unwrap_or(200), 0xfffe);
    probe.set_speed(SpeedConfig::khz(khz).unwrap())?;

    probe.swj_seq()?;

    let dpidr = probe.raw_read(Port::Debug, 0b0000)?;
    println!("DPIDR=0x{:08X}", dpidr);

    // Clear ABORT
    probe.raw_write(Port::Debug, 0b0000, 0x1e)?;

    let errmask = 0b10100010;
    let ctrl_stat = probe.raw_read(Port::Debug, 0b0100)?;
    println!("CTRL/STAT=0x{:08X}", ctrl_stat);

    if ctrl_stat & errmask != 0 {
        eprintln!("errors bits set in CTRL/STAT");
    }

    Ok(())
}
