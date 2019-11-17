use jaylink::*;

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
            "Bus {:03} Address {:03} Port {:03}: VID={:04x} PID={:04X} – ",
            devinfo.bus_number(),
            devinfo.address(),
            devinfo.port_number(),
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
                dev_data = format!("<error accessing device: {}>", e.to_string());
                None
            }
        };

        println!("{}", dev_data);

        if let Some(dev) = dev {
            // Print detailed information read from the device
            let info = match detailed_info(&dev) {
                Ok(info) => info,
                Err(e) => format!("<error accessing device: {}>", e),
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

fn detailed_info(dev: &JayLink) -> Result<String> {
    let caps = dev.read_capabilities()?;
    let firmware = dev.read_firmware_version()?;
    let hw_vers = dev.read_hardware_version()?;
    let speeds = dev.read_speeds()?;
    let max_mem_block = dev.read_max_mem_block()?;
    let intf = dev.read_current_interface()?;
    let avail_intfs = dev
        .read_available_interfaces()?
        .map(|intf| intf.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let tgt_voltage = dev.read_target_voltage()?;

    let mut info = String::new();
    info += &format!(" Capabilities: {:?}\n", caps);
    info += &format!("     Firmware: {}\n", firmware);
    info += &format!("   HW Version: {}\n", hw_vers);
    info += &format!("       Speeds: {:?}\n", speeds);
    info += &format!("Max. Memblock: {} bytes\n", max_mem_block);
    info += &format!("    Interface: {}\n", intf);
    info += &format!("Avail. Interf: {}\n", avail_intfs);
    info += &format!("        VTref: {} V\n", tgt_voltage as f32 / 1000.0);

    Ok(info)
}
