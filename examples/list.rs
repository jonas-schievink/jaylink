use jaylink::*;
use std::fmt::Write;

fn main() {
    env_logger::init();

    if let Err(e) = run() {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let list = jaylink::scan_usb()?.collect::<Vec<_>>();
    println!(
        "Found {} J-Link device{}.",
        list.len(),
        if list.len() == 1 { "" } else { "s" },
    );

    for devinfo in list {
        println!();
        print!(
            "Bus {:03} Address {:03}: VID={:04x} PID={:04X} – ",
            devinfo.bus_number(),
            devinfo.address(),
            devinfo.vid(),
            devinfo.pid(),
        );

        let dev_data;
        let dev = match devinfo.open() {
            Ok(dev) => {
                dev_data = format!(
                    "{} {} (Serial {})",
                    dev.manufacturer_string(),
                    dev.product_string(),
                    dev.serial_string(),
                );
                Some(dev)
            }
            Err(e) => {
                dev_data = format!("<{}>", e.to_string());
                None
            }
        };

        println!("{}", dev_data);

        if let Some(mut dev) = dev {
            // Print detailed information read from the device
            let info = match detailed_info(&mut dev) {
                Ok(info) => info,
                Err(e) => format!("<{}>", e),
            };

            // This is multi-line info
            for (i, line) in info.lines().enumerate() {
                if i == 0 {
                    print!("⤷ ");
                } else {
                    print!("  ");
                }

                println!("{}", line);
            }
        }
    }

    Ok(())
}

fn detailed_info(dev: &mut JayLink) -> Result<String> {
    let caps = dev.capabilities();
    let firmware = dev.read_firmware_version()?;
    let hw_vers = dev.read_hardware_version()?;
    let swo_speeds = dev.read_swo_speeds(SwoMode::Uart)?;
    let max_mem_block = dev.read_max_mem_block()?;
    let avail_intfs = dev.available_interfaces();
    let tgt_voltage = dev.read_target_voltage()?;

    let mut info = String::new();
    writeln!(info, "Capabilities: {:?}", caps).unwrap();
    writeln!(info, "Firmware: {}", firmware).unwrap();
    writeln!(info, "HW Version: {}", hw_vers).unwrap();
    writeln!(info, "Max. SWO Speed: {:?} Hz", swo_speeds.max_speed_hz()).unwrap();
    writeln!(info, "Max. Memblock: {} bytes", max_mem_block).unwrap();
    writeln!(info, "VTref: {} V", tgt_voltage as f32 / 1000.0).unwrap();
    writeln!(info, "Interfaces:").unwrap();
    for interface in avail_intfs {
        if interface == Interface::Fine {
            // FIXME: Selecting FINE hangs the probe.
            writeln!(info, "    - {} (skipped)", interface).unwrap();
            continue;
        }

        dev.select_interface(interface)?;
        let mhz = dev.read_speeds()?.max_speed_hz() / 1_000_000;
        writeln!(info, "    - {} (up to {} MHz)", interface, mhz).unwrap();
    }

    Ok(info)
}
