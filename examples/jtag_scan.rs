//! A small example showcasing how to auto-detect devices on a JTAG scan chain.
//!
//! This will enumerate JTAG devices and print their manufacturer and identification, as well as
//! JTAG tap properties.
//!
//! This is mostly a port of [Glasgow]'s `jtag_probe` module.
//!
//! [Glasgow]: https://github.com/GlasgowEmbedded/glasgow

use std::{cmp, iter};

use jaylink::{BitIter, Interface, JayLink, SpeedConfig};
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

    /// Maximum length of JTAG chain to expect (in bits).
    #[structopt(long = "max-length", default_value = "512")]
    max_length: usize,
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

    probe.select_interface(Interface::Jtag)?;

    // Limit speed so invalid 0xffff doesn't appear
    let khz = cmp::min(opts.speed, 0xfffe);
    probe.set_speed(SpeedConfig::khz(khz).unwrap())?;

    // Reset TAPs
    probe.reset_trst()?;

    let mut probe = JtagProbe {
        probe,
        state: State::TestLogicReset,
    };
    probe.enter_test_logic_reset()?;

    println!("Checking for JTAG connection...");
    probe.check_connection(opts.max_length)?;
    println!("✓ Scan chain detected.");

    println!("Scanning BYPASS chain length...");
    let dr = probe.scan_dr(opts.max_length)?;
    println!("✓ {} devices on the scan chain", dr.len());

    println!("Scanning instruction register length...");
    let ir = probe.scan_ir(opts.max_length)?;
    println!("✓ Total IR length: {} bits", ir);

    probe.enter_test_logic_reset()?; // Enable IDCODE instruction

    println!("Scanning IDCODE chain length...");
    let idcodes = probe.scan_dr(opts.max_length)?;
    println!("✓ Total IDCODE length: {} bits", idcodes.len());

    println!("Parsing IDCODEs...");
    let devices = separate_idcodes(&idcodes)?;
    let devs_id = devices.iter().filter(|d| d.is_some()).count();
    let devs_un = devices.iter().filter(|d| d.is_none()).count();
    println!("✓ {} devices with IDCODE, {} without:", devs_id, devs_un);

    for dev in devices {
        match dev {
            Some(idcode) => {
                let manufacturer = idcode
                    .manufacturer_name()
                    .map(|name| format!(" ({})", name))
                    .unwrap_or_else(|| " [unknown]".to_string());
                println!(
                    "- Manufacturer {:#06x}{}, Part Number {:#06x}, Version {}",
                    idcode.manufacturer(),
                    manufacturer,
                    idcode.part_number(),
                    idcode.version(),
                );
            }
            None => {
                println!("<device without IDCODE>");
            }
        }
    }

    Ok(())
}

/// Keeps track of the JTAG state machine, and provides methods to traverse it.
struct JtagProbe {
    probe: JayLink,
    state: State,
}

/// States of the JTAG state machine.
///
/// The JTAG state machine is controlled through the TMS signal (simultanously in all devices in
/// the scan chain).
///
/// This does not model *all* states, since this is just a demo.
#[derive(Copy, Clone, Debug)]
enum State {
    /// Initial state. Reached by shifting 5 or more 1-bits through TMS.
    ///
    /// Entering this state resets the TAP to the IDCODE instruction if implemented, or BYPASS if
    /// not. Since the specific bit pattern for IDCODE is implementation-defined, this is the only
    /// portable way to execute that instruction.
    TestLogicReset,

    /// Run-Test/Idle state.
    RunTestIdle,

    /// Shift bits into the Instruction Registers.
    ShiftIR,
    Exit1IR,

    /// Shift bits into the Data Registers.
    ShiftDR,
    Exit1DR,
}

impl State {
    fn set(&mut self, to: Self) {
        log::trace!("{:?} -> {:?}", self, to);
        *self = to;
    }
}

impl JtagProbe {
    /// Sends the bits in `tms` to the TMS input of the first device in the chain, while sending
    /// 0-bits through TDI.
    fn shift_tms(&mut self, tms: &[bool]) -> Result<()> {
        self.probe
            .jtag_io(tms.iter().copied(), iter::repeat(false).take(tms.len()))?;
        Ok(())
    }

    fn enter_test_logic_reset(&mut self) -> Result<()> {
        // Reset the JTAG state machine to Test-Logic-Reset. Sending enough (5 or more) 1-bits
        // through TMS will eventually end up in that state, regardless of what the current state
        // is.
        self.shift_tms(&[true, true, true, true, true])?;
        self.state.set(State::TestLogicReset);
        Ok(())
    }

    fn enter_run_test_idle(&mut self) -> Result<()> {
        let tms: &[_] = match self.state {
            State::RunTestIdle => return Ok(()),
            State::TestLogicReset => &[false],
            State::ShiftIR | State::ShiftDR => unreachable!(),
            State::Exit1IR | State::Exit1DR => &[true, false],
        };

        self.state.set(State::RunTestIdle);
        self.shift_tms(tms)
    }

    fn enter_shift_dr(&mut self) -> Result<()> {
        let tms: &[_] = match self.state {
            State::ShiftDR => return Ok(()),
            State::TestLogicReset => &[false, true, false, false],
            State::RunTestIdle => &[true, false, false],
            State::Exit1IR | State::Exit1DR | State::ShiftIR => unreachable!("{:?}", self.state),
        };

        self.state.set(State::ShiftDR);
        self.shift_tms(tms)
    }

    fn enter_shift_ir(&mut self) -> Result<()> {
        let tms: &[_] = match self.state {
            State::ShiftIR => return Ok(()),
            State::TestLogicReset => &[false, true, true, false, false],
            State::RunTestIdle => &[true, true, false, false],
            State::Exit1IR | State::Exit1DR | State::ShiftDR => unreachable!("{:?}", self.state),
        };

        self.state.set(State::ShiftIR);
        self.shift_tms(tms)
    }

    /// Shifts data into TDI while in a `Shift-*` state.
    ///
    /// `tdi`: Bits to shift into TDI.
    /// `leave`: Whether to leave the current state by sending a single TMS=1 pulse with the last bit.
    fn shift_tdio(&mut self, tdi: &[bool], leave: bool) -> Result<BitIter<'_>> {
        let tms = iter::repeat(false)
            .take(tdi.len() - 1)
            .chain(iter::once(leave));
        let tdo = self.probe.jtag_io(tms, tdi.iter().copied())?;

        if leave {
            match self.state {
                State::ShiftIR => self.state.set(State::Exit1IR),
                State::ShiftDR => self.state.set(State::Exit1DR),
                _ => unreachable!("{:?}", self.state),
            };
        }

        Ok(tdo)
    }

    /// Checks whether a closed JTAG scan chain is established and loads BYPASS into every TAPs
    /// instruction register.
    fn check_connection(&mut self, max_length: usize) -> Result<()> {
        self.enter_shift_ir()?;

        let mut broken = false;
        let res = (|| {
            // Keep shifting 0s through the chain. Once filled up (assumed to happen at at most
            // `max_length` bits), we should see a continuous stream of 0s in TDO.
            self.shift_tdio(&vec![false; max_length], false)?;
            let mut zeroes = self.shift_tdio(&vec![false; max_length], false)?;
            log::trace!("check_connection: zeroes = {:?}", zeroes);
            if zeroes.any(|bit| bit) {
                broken = true;
            }

            // Now do the same with 1s, in case TDO is just floating low (or has a pull-down).
            self.shift_tdio(&vec![true; max_length], false)?;
            let mut ones = self.shift_tdio(&vec![true; max_length], true)?;
            log::trace!("check_connection: ones = {:?}", ones);
            if ones.any(|bit| !bit) {
                broken = true;
            }

            // Every device in the chain is now in BYPASS mode.

            Ok(())
        })();

        self.enter_run_test_idle()?;
        if broken {
            return Err(
                "could not detect closed JTAG chain (check connection, or consider increasing \
                the max chain length)"
                    .into(),
            );
        }
        res
    }

    /// Scans the data register contents and determines their total length.
    ///
    /// Returns the old DR contents.
    ///
    /// Note that DR length can change depending on the active instruction.
    fn scan_dr(&mut self, max_length: usize) -> Result<Vec<bool>> {
        self.enter_shift_dr()?;

        let mut original_contents = None;
        let res = (|| -> Result<_> {
            // Fill all data registers with 1s, then flush with 0s and see how many 1-bits were
            // retained.
            let data_1 = self.shift_tdio(&vec![true; max_length], false)?;
            original_contents = Some(data_1.collect::<Vec<_>>());
            let mut data_0 = self.shift_tdio(&vec![false; max_length], false)?;
            match data_0.position(|bit| !bit) {
                Some(i) => Ok(i),
                None => Err(format!("total DR length longer than {} bits", max_length).into()),
            }
        })();
        // Restore old DR contents (received in `data_1`).
        let mut orig = original_contents.unwrap_or_else(Vec::new);
        self.shift_tdio(&orig, true)?;
        self.enter_run_test_idle()?;
        let dr_len = res?;
        orig.truncate(dr_len);
        Ok(orig)
    }

    fn scan_ir(&mut self, max_length: usize) -> Result<usize> {
        self.enter_shift_ir()?;

        let res = (|| {
            // Fill all instruction registers with 1s, then flush with 0s and see how many 1-bits
            // were retained.
            let _data_1 = self.shift_tdio(&vec![true; max_length], false)?;
            let mut data_0 = self.shift_tdio(&vec![false; max_length], false)?;
            match data_0.position(|bit| !bit) {
                Some(i) => Ok(i),
                None => Err(format!("total IR length longer than {} bits", max_length).into()),
            }
        })();
        // Fill IRs with BYPASS instructions.
        self.shift_tdio(&vec![true; max_length], true)?;
        self.enter_run_test_idle()?;
        res
    }
}

struct IdCode(u32);

impl IdCode {
    fn version(&self) -> u8 {
        (self.0 >> 28) as u8
    }

    fn part_number(&self) -> u16 {
        (self.0 >> 12) as u16
    }

    fn manufacturer(&self) -> u16 {
        ((self.0 >> 1) & 0b111_1111_1111) as u16
    }

    /// Returns the JEDEC Continuation Code page, modulo 16.
    fn manufacturer_jedec_cc(&self) -> u8 {
        ((self.manufacturer() >> 7) & 0b1111) as u8
    }

    fn manufacturer_jedec_id(&self) -> u8 {
        self.manufacturer() as u8 & 0x7F
    }

    fn manufacturer_name(&self) -> Option<&'static str> {
        // NOTE: JTAG standard says that the `cc` field is modulo-16 if the real `cc` value is >15.
        // Luckily JEP106 only has 11 banks or so, so we conveniently ignore that problem for now.
        jep106::JEP106Code::new(self.manufacturer_jedec_cc(), self.manufacturer_jedec_id()).get()
    }
}

/// Parses data register contents containing IDCODE or BYPASS registers in reset (0) state.
fn separate_idcodes(idcodes: &[bool]) -> Result<Vec<Option<IdCode>>> {
    // The LSb (transmitted first) of IDCODE is 1, while BYPASS registers are 0 upon reset, which
    // allows us to separate them here.
    let mut devices = Vec::new();
    let mut index = 0;
    while index < idcodes.len() {
        match idcodes[index] {
            false => {
                // BYPASS
                devices.push(None);
            }
            true => {
                // IDCODE
                let mut idcode: u32 = 0x8000_0000;
                for _ in 0..31 {
                    index += 1;
                    idcode >>= 1;
                    if idcodes[index] {
                        idcode |= 0x8000_0000;
                    }
                }
                assert_eq!(idcode & 1, 1);

                devices.push(Some(IdCode(idcode)));
            }
        }

        index += 1;
    }

    Ok(devices)
}
