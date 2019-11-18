//! A crate for talking to J-Link debug probes connected via USB.
//!
//! This crate allows access to the vendor-specific USB interface used to control JTAG / SWD
//! operations and other functionality. It does *not* provide access to the virtual COM port
//! functionality (which is a regular CDC device, so no special support is needed).
//!
//! Inspired by [libjaylink] (though this library is not a port).
//!
//! [libjaylink]: https://repo.or.cz/libjaylink.git
//!
//! # Pinout
//!
//! J-Link uses a pinout based on the standard 20-pin ARM JTAG connector, extended for SWD
//! compatibility and with pins for UART.
//!
//! JTAG pinout:
//!
//! ```notrust
//!            ┌───────────┐
//!     VTref  │ *  1  2 * │ NC
//!     nTRST  │ *  3  4 * │ GND
//!       TDI  │ *  5  6 * │ GND
//!       TMS  │ *  7  8 * │ GND
//!       TCK ┌┘ *  9 10 * │ GND
//!      RTCK └┐ * 11 12 * │ GND
//!       TDO  │ * 13 14 * │ GND
//!     RESET  │ * 15 16 * │ GND
//!     DBGRQ  │ * 17 18 * │ GND
//! 5V-Supply  │ * 19 20 * │ GND
//!            └───────────┘
//! ```
//!
//! SWD (+ UART) pinout:
//!
//! ```notrust
//!            ┌───────────┐
//!     VTref  │ *  1  2 * │ NC
//!         -  │ *  3  4 * │ GND
//! J-Link TX  │ *  5  6 * │ GND
//!     SWDIO  │ *  7  8 * │ GND
//!     SWCLK ┌┘ *  9 10 * │ GND
//!         - └┐ * 11 12 * │ GND
//!       SWO  │ * 13 14 * │ GND
//!     RESET  │ * 15 16 * │ GND
//! J-Link RX  │ * 17 18 * │ GND
//! 5V-Supply  │ * 19 20 * │ GND
//!            └───────────┘
//! ```
//!
//! # Reference
//!
//! Segger has released a PDF documenting the USB protocol: "Reference manual for J-Link USB
//! Protocol" (Document RM08001-R2).
//!
//! The archive.org version is the most up-to-date one.

#![doc(html_root_url = "https://docs.rs/jaylink/0.0.0")]
// Deny a few warnings in doctests, since rustdoc `allow`s many warnings by default
#![doc(test(attr(deny(unused_imports, unused_must_use))))]
#![warn(missing_debug_implementations, rust_2018_idioms)]

mod capabilities;
mod error;
mod readme;

mod private {
    /// Used in `__NonExhaustive` variants to make them unconstructible.
    ///
    /// Users are still able to match on them, unfortunately.
    #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Private {}
}

pub use bitvec;

pub use self::capabilities::Capabilities;
pub use self::error::{Error, ErrorKind};

pub(crate) use self::error::ResultExt as _;
use bitflags::bitflags;
use bitvec::{cursor, slice::BitSlice};
use byteorder::{LittleEndian, ReadBytesExt};
use log::{debug, trace, warn};
use std::cell::{Cell, RefCell, RefMut};
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

/// A result type with the error hardwired to [`Error`].
///
/// [`Error`]: struct.Error.html
pub type Result<T> = std::result::Result<T, Error>;

const VID_SEGGER: u16 = 0x1366;

const TIMEOUT_DEFAULT: Duration = Duration::from_millis(500);

#[repr(u8)]
#[allow(dead_code)]
enum Command {
    Version = 0x01,
    GetSpeeds = 0xC0,
    GetMaxMemBlock = 0xD4,
    GetCaps = 0xE8,
    GetCapsEx = 0xED,
    GetHwVersion = 0xF0,

    GetState = 0x07,
    GetHwInfo = 0xC1,
    GetCounters = 0xC2,
    MeasureRtckReact = 0xF6,

    ResetTrst = 0x02,
    SetSpeed = 0x05,
    SelectIf = 0xC7,
    SetKsPower = 0x08,
    HwClock = 0xC8,
    HwTms0 = 0xC9,
    HwTms1 = 0xCA,
    HwData0 = 0xCB,
    HwData1 = 0xCC,
    HwJtag = 0xCD,
    HwJtag2 = 0xCE,
    HwJtag3 = 0xCF,
    HwJtagWrite = 0xD5,
    HwJtagGetResult = 0xD6,
    HwTrst0 = 0xDE,
    HwTrst1 = 0xDF,
    WriteDcc = 0xF1,

    ResetTarget = 0x03,
    HwReleaseResetStopEx = 0xD0,
    HwReleaseResetStopTimed = 0xD1,
    HwReset0 = 0xDC,
    HwReset1 = 0xDD,
    GetCpuCaps = 0xE9,
    ExecCpuCmd = 0xEA,
    WriteMem = 0xF4,
    ReadMem = 0xF5,
    WriteMemArm79 = 0xF7,
    ReadMemArm79 = 0xF8,

    ReadConfig = 0xF2,
    WriteConfig = 0xF3,
}

/// A handle to a J-Link USB device.
///
/// This is the main interface type of this library. There are multiple ways of obtaining an
/// instance of it:
///
/// * [`JayLink::open_by_serial`]: Either opens the only J-Link device connected to the computer, or
///   opens a specific one by its serial number. Recommended for applications that interact with one
///   J-Link device only (ie. most of them).
/// * [`JayLink::open_usb`]: Opens a specific J-Link device according to the given
///   [`UsbDeviceInfo`]. Also see [`scan_usb`].
///
/// [`JayLink::open_by_serial`]: struct.JayLink.html#method.open_by_serial
/// [`JayLink::open_usb`]: struct.JayLink.html#method.open_usb
/// [`UsbDeviceInfo`]: struct.UsbDeviceInfo.html
/// [`scan_usb`]: fn.scan_usb.html
pub struct JayLink {
    handle: rusb::DeviceHandle<rusb::GlobalContext>,

    read_ep: u8,
    write_ep: u8,
    cmd_buf: RefCell<Vec<u8>>,
    /// The capabilities reported by the device. They're fetched lazily and are globally cached
    /// since they don't change while connected to the device (hopefully!).
    caps: Cell<Option<Capabilities>>,
    /// The currently selected target interface. This is cached to avoid unnecessary roundtrips when
    /// performing JTAG/SWD operations.
    interface: Cell<Option<Interface>>,

    manufacturer: String,
    product: String,
    serial: String,
}

impl JayLink {
    /// Opens an attached J-Link device by its serial number.
    ///
    /// If `serial` is `None`, this will open the only attached J-Link device, and return an error
    /// of type [`MultipleDevicesFound`] when more than one is attached. This is usually the desired
    /// behavior of robust applications.
    ///
    /// [`MultipleDevicesFound`]: enum.ErrorKind.html#variant.MultipleDevicesFound
    pub fn open_by_serial(serial: Option<&str>) -> Result<Self> {
        let mut devices = scan_usb()?.filter_map(|usb_device| {
            let dev = match usb_device.open() {
                Ok(dev) => dev,
                Err(_) => return None,
            };

            if let Some(serial) = serial {
                if dev.serial_string() == serial {
                    Some(dev)
                } else {
                    None
                }
            } else {
                Some(dev)
            }
        });

        let first = devices.next().ok_or_else(|| {
            let message = if let Some(serial ) = serial {
                format!("no J-Link device with serial {} were found (make sure your current user has permissions to access them)", serial)
            } else {
                "no J-Link devices found (make sure your current user has permissions to access them)".to_string()
            };
            Error::new(ErrorKind::DeviceNotFound, message)
        })?;

        if devices.next().is_some() {
            let msg = if let Some(serial) = serial {
                format!("found multiple devices matching serial {}", serial)
            } else {
                "multiple devices found (specify serial number to select one)".to_string()
            };
            return Err(Error::new(ErrorKind::MultipleDevicesFound, msg));
        }

        Ok(first)
    }

    /// Opens a specific J-Link USB device.
    pub fn open_usb(usb_device: UsbDeviceInfo) -> Result<Self> {
        // NB: We take `UsbDeviceInfo` by value since it isn't cloneable (yet), so taking it by-ref
        // would lock us into a less flexible API. It should be easy to make it cloneable with a few
        // changes to rusb though.

        let descr = usb_device
            .inner
            .device_descriptor()
            .expect("libusb_get_device_descriptor returned unexpected error");
        let mut handle = usb_device.inner.open().jaylink_err()?;

        debug!("open_usb: device descriptor: {:#x?}", descr);

        if descr.num_configurations() != 1 {
            warn!(
                "device has {} configurations, expected 1",
                descr.num_configurations()
            );
        }

        let conf = handle.active_configuration().jaylink_err()?;
        // Device configurations are 1-indexed, apparently
        if conf != 1 {
            warn!(
                "device in configuration {}, expected 1; changing configuration",
                conf
            );
            handle.set_active_configuration(1).jaylink_err()?;
        }

        let conf = usb_device.inner.active_config_descriptor().jaylink_err()?;
        debug!("scanning {} interfaces", conf.num_interfaces());
        trace!("active configuration descriptor: {:#x?}", conf);

        let mut jlink_intf = None;
        for (i, intf) in conf.interfaces().enumerate() {
            trace!("interface #{} descriptors:", i + 1);

            for descr in intf.descriptors() {
                trace!("{:#x?}", descr);

                // We detect the proprietary J-Link interface using the vendor-specific class codes
                // and the endpoint properties
                if descr.class_code() == 0xff
                    && descr.sub_class_code() == 0xff
                    && descr.protocol_code() == 0xff
                {
                    if let Some((intf, _, _)) = jlink_intf {
                        return Err(format!(
                            "found multiple matching USB interfaces ({} and {})",
                            intf,
                            descr.interface_number()
                        ))
                        .jaylink_err();
                    }

                    let endpoints: Vec<_> = descr.endpoint_descriptors().collect();
                    trace!("endpoint descriptors: {:#x?}", endpoints);
                    if endpoints.len() != 2 {
                        warn!("vendor-specific interface with {} endpoints, expected 2 (skipping interface)", endpoints.len());
                        continue;
                    }

                    if !endpoints
                        .iter()
                        .all(|ep| ep.transfer_type() == rusb::TransferType::Bulk)
                    {
                        warn!(
                            "encountered non-bulk endpoints, skipping interface: {:#x?}",
                            endpoints
                        );
                        continue;
                    }

                    let (read_ep, write_ep) = if endpoints[0].direction() == rusb::Direction::In {
                        (endpoints[0].address(), endpoints[1].address())
                    } else {
                        (endpoints[1].address(), endpoints[0].address())
                    };

                    jlink_intf = Some((descr.interface_number(), read_ep, write_ep));
                    debug!("J-Link interface is #{}", descr.interface_number());
                }
            }
        }

        let (intf, read_ep, write_ep) = if let Some(intf) = jlink_intf {
            intf
        } else {
            return Err("device is not a J-Link device".to_string()).jaylink_err();
        };

        handle.claim_interface(intf).jaylink_err()?;

        // Check that we're still in the expected configuration (another application could
        // interfere).
        // See: http://libusb.sourceforge.net/api-1.0/caveats.html
        let conf = handle.active_configuration().jaylink_err()?;
        if conf != 1 {
            return Err("another application is accessing the device".to_string()).jaylink_err();
        }

        Ok(Self {
            manufacturer: handle
                .read_manufacturer_string_ascii(&descr)
                .jaylink_err()?,
            product: handle.read_product_string_ascii(&descr).jaylink_err()?,
            serial: handle
                .read_serial_number_string_ascii(&descr)
                .jaylink_err()?,
            read_ep,
            write_ep,
            cmd_buf: RefCell::new(Vec::new()),
            caps: Cell::new(None),
            interface: Cell::new(None),
            handle,
        })
    }

    /// Returns the manufacturer string stored in the device descriptor.
    pub fn manufacturer_string(&self) -> &str {
        &self.manufacturer
    }

    /// Returns the product string stored in the device descriptor.
    pub fn product_string(&self) -> &str {
        &self.product
    }

    /// Returns the serial number string stored in the device descriptor.
    ///
    /// This serial number string can be passed to [`JayLink::open_by_serial`] to open a specific
    /// J-Link device.
    ///
    /// [`JayLink::open_by_serial`]: #method.open_by_serial
    pub fn serial_string(&self) -> &str {
        &self.serial
    }

    fn buf(&self, len: usize) -> RefMut<'_, [u8]> {
        let mut vec = self.cmd_buf.borrow_mut();
        vec.resize(len, 0);
        RefMut::map(vec, |vec| &mut **vec)
    }

    fn write_cmd(&self, cmd: &[u8]) -> Result<()> {
        let bytes = self
            .handle
            .write_bulk(self.write_ep, cmd, TIMEOUT_DEFAULT)
            .jaylink_err()?;
        if bytes != cmd.len() {
            return Err(rusb::Error::Other).jaylink_err();
        }
        Ok(())
    }

    fn read(&self, buf: &mut [u8]) -> Result<()> {
        let bytes = self
            .handle
            .read_bulk(self.read_ep, buf, TIMEOUT_DEFAULT)
            .jaylink_err()?;
        if bytes != buf.len() {
            return Err(rusb::Error::Other).jaylink_err();
        }

        Ok(())
    }

    fn require_capabilities(&self, cap: Capabilities) -> Result<()> {
        let caps = self.read_capabilities()?;

        if caps.contains(cap) {
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::MissingCapability,
                format!("device is missing capabilities ({:?}) for operation", cap),
            ))
        }
    }

    fn has_capabilities(&self, cap: Capabilities) -> Result<bool> {
        let caps = self.read_capabilities()?;

        Ok(caps.contains(cap))
    }

    /// Reads the firmware version string from the device.
    pub fn read_firmware_version(&self) -> Result<String> {
        self.write_cmd(&[Command::Version as u8])?;

        let mut buf = [0; 2];
        self.read(&mut buf)?;
        let num_bytes = u16::from_le_bytes(buf);
        let mut buf = self.buf(num_bytes.into());
        let mut buf = &mut buf[..usize::from(num_bytes)];
        self.read(&mut buf)?;

        Ok(String::from_utf8_lossy(buf).to_string())
    }

    /// Reads the hardware version from the device.
    ///
    /// This requires the [`GET_HW_VERSION`] capability.
    ///
    /// [`GET_HW_VERSION`]: struct.Capabilities.html#associatedconstant.GET_HW_VERSION
    pub fn read_hardware_version(&self) -> Result<HardwareVersion> {
        self.require_capabilities(Capabilities::GET_HW_VERSION)?;

        self.write_cmd(&[Command::GetHwVersion as u8])?;

        let mut buf = [0; 4];
        self.read(&mut buf)?;

        Ok(HardwareVersion::from_u32(u32::from_le_bytes(buf)))
    }

    /// Read the probe's CPU speed information.
    ///
    /// This requires the [`SPEED_INFO`] capability.
    ///
    /// [`SPEED_INFO`]: struct.Capabilities.html#associatedconstant.SPEED_INFO
    pub fn read_speeds(&self) -> Result<Speeds> {
        self.require_capabilities(Capabilities::SPEED_INFO)?;

        self.write_cmd(&[Command::GetSpeeds as u8])?;

        let mut buf = [0; 6];
        self.read(&mut buf)?;
        let mut buf = &buf[..];

        Ok(Speeds {
            base_freq: buf.read_u32::<LittleEndian>().unwrap(),
            min_div: buf.read_u16::<LittleEndian>().unwrap(),
        })
    }

    /// Reads the maximum mem block size in Bytes.
    ///
    /// This requires the [`GET_MAX_BLOCK_SIZE`] capability.
    ///
    /// [`GET_MAX_BLOCK_SIZE`]: struct.Capabilities.html#associatedconstant.GET_MAX_BLOCK_SIZE
    pub fn read_max_mem_block(&self) -> Result<u32> {
        // This cap refers to a nonexistent command `GET_MAX_BLOCK_SIZE`, but it probably means
        // `GET_MAX_MEM_BLOCK`.
        self.require_capabilities(Capabilities::GET_MAX_BLOCK_SIZE)?;

        self.write_cmd(&[Command::GetMaxMemBlock as u8])?;

        let mut buf = [0; 4];
        self.read(&mut buf)?;

        Ok(u32::from_le_bytes(buf))
    }

    /// Reads the advertised capabilities from the device.
    pub fn read_capabilities(&self) -> Result<Capabilities> {
        if let Some(caps) = self.caps.get() {
            Ok(caps)
        } else {
            self.write_cmd(&[Command::GetCaps as u8])?;

            let mut buf = [0; 4];
            self.read(&mut buf)?;

            let mut caps = Capabilities::from_raw_legacy(u32::from_le_bytes(buf));
            debug!("legacy caps: {:?}", caps);

            // If the `GET_CAPS_EX` capability is set, use the extended capability command to fetch
            // all the capabilities.
            if caps.contains(Capabilities::GET_CAPS_EX) {
                self.write_cmd(&[Command::GetCapsEx as u8])?;

                let mut buf = [0; 32];
                self.read(&mut buf)?;
                let real_caps = Capabilities::from_raw_ex(buf);
                if !real_caps.contains(caps) {
                    return Err(format!(
                        "ext. caps are not a superset of legacy caps (legacy: {:?}, ex: {:?})",
                        caps, real_caps
                    ))
                    .jaylink_err();
                }
                debug!("extended caps: {:?}", real_caps);
                caps = real_caps;
            } else {
                debug!("extended caps not supported");
            }

            self.caps.set(Some(caps));
            Ok(caps)
        }
    }

    /// Changes the state of the TMS / SWDIO pin (pin 7).
    ///
    /// The pin will be set to the level of `VTref` if `tms` is `true`, and to GND if it is `false`.
    ///
    /// **Note**: On some hardware, detaching `VTref` might not affect the internal reading, so the
    /// old level might still be used afterwards.
    pub fn set_tms(&mut self, tms: bool) -> Result<()> {
        let cmd = if tms {
            Command::HwTms1
        } else {
            Command::HwTms0
        };
        self.write_cmd(&[cmd as u8])
    }

    /// Changes the state of the TDI / TX pin (pin 5).
    ///
    /// The pin will be set to the level of `VTref` if `tdi` is `true`, and to GND if it is `false`.
    ///
    /// **Note**: On some hardware, detaching `VTref` might not affect the internal reading, so the
    /// old level might still be used afterwards.
    pub fn set_tdi(&mut self, tdi: bool) -> Result<()> {
        let cmd = if tdi {
            Command::HwData1
        } else {
            Command::HwData0
        };
        self.write_cmd(&[cmd as u8])
    }

    /// Changes the state of the (n)TRST pin (pin 3).
    ///
    /// The pin will be set to the level of `VTref` if `trst` is `true`, and to GND if it is
    /// `false`.
    ///
    /// **Note**: On some hardware, detaching `VTref` might not affect the internal reading, so the
    /// old level might still be used afterwards.
    ///
    /// **Note**: Some embedded J-Link probes may not expose this pin or may not allow controlling
    /// it using this function.
    pub fn set_trst(&mut self, trst: bool) -> Result<()> {
        let cmd = if trst {
            Command::HwTrst1
        } else {
            Command::HwTrst0
        };
        self.write_cmd(&[cmd as u8])
    }

    /// Changes the state of the RESET pin (pin 15).
    ///
    /// RESET is an open-collector / open-drain output. If `reset` is `true`, the output will float.
    /// If `reset` is `false`, the output will be pulled to ground.
    ///
    /// **Note**: Some embedded J-Link probes may not expose this pin or may not allow controlling
    /// it using this function.
    pub fn set_reset(&mut self, reset: bool) -> Result<()> {
        let cmd = if reset {
            Command::HwReset1
        } else {
            Command::HwReset0
        };
        self.write_cmd(&[cmd as u8])
    }

    /// Resets the target's JTAG TAP controller by temporarily asserting (n)TRST (Pin 3).
    pub fn reset_trst(&mut self) -> Result<()> {
        self.write_cmd(&[Command::ResetTrst as u8])
    }

    /// Resets the target by temporarily asserting the RESET pin (pin 15).
    pub fn reset_target(&mut self) -> Result<()> {
        self.write_cmd(&[Command::ResetTarget as u8])
    }

    /// Reads the currently selected target interface.
    ///
    /// This requires the [`SELECT_IF`] capability.
    ///
    /// [`SELECT_IF`]: struct.Capabilities.html#associatedconstant.SELECT_IF
    pub fn read_current_interface(&self) -> Result<Interface> {
        if let Some(intf) = self.interface.get() {
            Ok(intf)
        } else {
            self.require_capabilities(Capabilities::SELECT_IF)?;

            self.write_cmd(&[Command::SelectIf as u8, 0xFE])?;

            let mut buf = [0; 4];
            self.read(&mut buf)?;

            let raw = u32::from_le_bytes(buf);
            let intf = Interface::from_u32(raw)
                .ok_or_else(|| format!("invalid interface value {}", raw))
                .jaylink_err()?;
            debug!("read active interface: {:?}", intf);
            self.interface.set(Some(intf));
            Ok(intf)
        }
    }

    /// Reads the list of available target interfaces that can be selected.
    ///
    /// This requires the [`SELECT_IF`] capability.
    ///
    /// [`SELECT_IF`]: struct.Capabilities.html#associatedconstant.SELECT_IF
    pub fn read_available_interfaces(&self) -> Result<impl Iterator<Item = Interface>> {
        self.require_capabilities(Capabilities::SELECT_IF)?;

        self.write_cmd(&[Command::SelectIf as u8, 0xFF])?;

        let mut buf = [0; 4];
        self.read(&mut buf)?;

        let intfs = Interfaces::from_bits_truncate(u32::from_le_bytes(buf));
        Ok(intfs.into_iter())
    }

    /// Selects the interface to use for talking to the target MCU.
    ///
    /// This requires the [`SELECT_IF`] capability.
    ///
    /// [`SELECT_IF`]: struct.Capabilities.html#associatedconstant.SELECT_IF
    pub fn select_interface(&mut self, intf: Interface) -> Result<()> {
        if self.interface.get() == Some(intf) {
            return Ok(());
        }

        self.require_capabilities(Capabilities::SELECT_IF)?;

        self.write_cmd(&[Command::SelectIf as u8, intf.as_u8()])?;

        // Returns the previous interface, ignore it
        let mut buf = [0; 4];
        self.read(&mut buf)?;

        self.interface.set(Some(intf));

        Ok(())
    }

    /// Sets the target communication speed.
    ///
    /// If `speed` is set to [`CommunicationSpeed::ADAPTIVE`], then the [`ADAPTIVE_CLOCKING`]
    /// capability is required.
    ///
    /// [`CommunicationSpeed::ADAPTIVE`]: struct.CommunicationSpeed.html#associatedconstant.ADAPTIVE
    /// [`ADAPTIVE_CLOCKING`]: struct.Capabilities.html#associatedconstant.ADAPTIVE_CLOCKING
    pub fn set_speed(&mut self, speed: CommunicationSpeed) -> Result<()> {
        if speed.raw == CommunicationSpeed::ADAPTIVE.raw {
            self.require_capabilities(Capabilities::ADAPTIVE_CLOCKING)?;
        }

        let mut buf = [Command::SetSpeed as u8, 0, 0];
        buf[1..3].copy_from_slice(&speed.raw.to_le_bytes());
        self.write_cmd(&buf)?;
        Ok(())
    }

    /// Reads the target voltage measured on the `VTref` pin, in millivolts.
    pub fn read_target_voltage(&self) -> Result<u16> {
        self.write_cmd(&[Command::GetState as u8])?;

        let mut buf = [0; 8];
        self.read(&mut buf)?;

        let voltage = [buf[0], buf[1]];
        Ok(u16::from_le_bytes(voltage))
    }

    /// Enable or disable the 5V Power supply on pin 19.
    ///
    /// This requires the [`SET_KS_POWER`] capability.
    ///
    /// **Note**: The startup state of the power supply can be configured in non-volatile memory.
    ///
    /// **Note**: Some embedded J-Links may not provide this feature or do not have the 5V supply
    /// routed to a pin.
    ///
    /// **Note**: The 5V supply is protected against overcurrent. Check the device manual for more
    /// information on this.
    ///
    /// [`SET_KS_POWER`]: struct.Capabilities.html#associatedconstant.SET_KS_POWER
    pub fn set_kickstart_power(&mut self, enable: bool) -> Result<()> {
        self.require_capabilities(Capabilities::SET_KS_POWER)?;
        self.write_cmd(&[Command::SetKsPower as u8, enable as u8])?;
        Ok(())
    }

    /// Performs a JTAG I/O operation.
    ///
    /// This will put the probe into JTAG interface mode, if JTAG isn't selected already.
    ///
    /// # Parameters
    ///
    /// * `tms`: TMS bits to transmit.
    /// * `tdi_tdo`: TDI bits to transmit, and TDO receive buffer.
    ///
    /// # Panics
    ///
    /// This method will panic if `tms` and `tdi_tdo` have different lengths. It will also panic if
    /// any of them contains more then 65535 bits of data.
    pub fn jtag_io(
        &mut self,
        tms: &BitSlice<cursor::LittleEndian, u8>,
        tdi_tdo: &mut BitSlice<cursor::LittleEndian, u8>,
    ) -> Result<()> {
        assert_eq!(
            tms.len(),
            tdi_tdo.len(),
            "TMS and TDI must have the same number of bits"
        );
        assert!(tms.len() < 65535, "too much data to transfer");

        // There's 3 commands for doing a JTAG transfer. The older 2 are obsolete with hardware
        // version 5 and above, which adds the 3rd command. Unfortunately we cannot reliably use the
        // HW version to determine this since some embedded J-Link probes have a HW version of
        // 1.0.0, but still support SWD, so we use the `SELECT_IF` capability instead.
        let cmd = if self.has_capabilities(Capabilities::SELECT_IF)? {
            // Use the new JTAG3 command, make sure to select the JTAG interface mode
            self.select_interface(Interface::Jtag)?;
            Command::HwJtag3
        } else {
            // Use the legacy JTAG2 command
            // FIXME is HW_JTAG relevant at all?
            Command::HwJtag2
        };

        // JTAG3 and JTAG2 use the same format for JTAG operations
        let num_bits = tms.len() as u16;
        let num_bytes = usize::from((num_bits + 7) >> 3);
        let mut buf = self.buf(1 + 1 + 2 + num_bytes * 2);
        buf[0] = cmd as u8;
        // buf[1] is dummy data for alignment
        buf[2..=3].copy_from_slice(&num_bits.to_le_bytes());
        buf[4..4 + tms.as_slice().len()].copy_from_slice(tms.as_slice());
        buf[4 + tms.as_slice().len()..][..tdi_tdo.as_slice().len()]
            .copy_from_slice(tdi_tdo.as_slice());

        self.write_cmd(&buf)?;

        // Response is `num_bytes` TDO data bytes and one status byte
        self.read(&mut buf[..num_bytes + 1])?;

        if buf[num_bytes] != 0 {
            return Err(format!("JTAG op returned error code {:#x}", buf[num_bytes])).jaylink_err();
        }

        tdi_tdo.as_mut_slice().copy_from_slice(&buf[..num_bytes]);
        Ok(())
    }

    /// Performs an SWD I/O operation.
    ///
    /// This will put the probe in SWD mode if it isn't already in that mode.
    ///
    /// This requires the [`SELECT_IF`] capability.
    ///
    /// # Parameters
    ///
    /// * `dir`: Transfer direction of the bit (0 = Input, 1 = Output).
    /// * `swdio`: SWD data bits.
    ///
    /// If `dir` is 1, the corresponding bit in `swdio` will be written to the target; if it is 0,
    /// the bit in `swdio` is ignored and a bit is read from the target instead.
    ///
    /// After this call returns, bits in `swdio` whose `dir` is 0 are replaced with the bit read
    /// from the target. Written bits (with a `dir` of 1), are undefined.
    ///
    /// [`SELECT_IF`]: struct.Capabilities.html#associatedconstant.SELECT_IF
    pub fn swd_io(
        &mut self,
        dir: &BitSlice<cursor::LittleEndian, u8>,
        swdio: &mut BitSlice<cursor::LittleEndian, u8>,
    ) -> Result<()> {
        assert_eq!(
            dir.len(),
            swdio.len(),
            "DIR and SWDIO must have the same number of bits"
        );
        assert!(dir.len() < 65535, "too much data to transfer");

        self.select_interface(Interface::Swd)?;

        let num_bits = dir.len() as u16;
        let num_bytes = usize::from((num_bits + 7) >> 3);
        let mut buf = self.buf(1 + 1 + 2 + num_bytes * 2);
        buf[0] = Command::HwJtag3 as u8;
        // buf[1] is dummy data for alignment
        buf[2..=3].copy_from_slice(&num_bits.to_le_bytes());
        buf[4..4 + num_bytes].copy_from_slice(dir.as_slice());
        buf[4 + num_bytes..4 + num_bytes * 2].copy_from_slice(swdio.as_slice());

        self.write_cmd(&buf)?;

        // Response is `num_bytes` TDO data bytes and one status byte
        self.read(&mut buf[..num_bytes + 1])?;

        if buf[num_bytes] != 0 {
            return Err(format!("JTAG op returned error code {:#x}", buf[num_bytes])).jaylink_err();
        }

        swdio.as_mut_slice().copy_from_slice(&buf[..num_bytes]);
        Ok(())
    }
}

impl fmt::Debug for JayLink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JayLink")
            .field("manufacturer", &self.manufacturer)
            .field("product", &self.product)
            .field("serial", &self.serial)
            .finish()
    }
}

/// Target communication speed setting.
///
/// This determines the clock frequency of the JTAG/SWD communication.
#[derive(Debug)]
pub struct CommunicationSpeed {
    raw: u16,
}

impl CommunicationSpeed {
    /// Let the J-Link probe decide the speed.
    ///
    /// Requires the [`ADAPTIVE_CLOCKING`] capability.
    ///
    /// [`ADAPTIVE_CLOCKING`]: struct.Capabilities.html#associatedconstant.ADAPTIVE_CLOCKING
    pub const ADAPTIVE: Self = Self { raw: 0xFFFF };

    /// Manually specify speed in kHz.
    ///
    /// Returns `None` if the value is the invalid value `0xFFFF`. Note that this doesn't mean that
    /// every other value will be accepted by the device.
    pub fn khz(khz: u16) -> Option<Self> {
        if khz == 0xFFFF {
            None
        } else {
            Some(Self { raw: khz })
        }
    }
}

/// List of target interfaces (JTAG / SWD).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Interface {
    /// JTAG interface.
    Jtag,
    /// SWD interface (Serial Wire Debug).
    Swd,

    #[doc(hidden)]
    __NonExhaustive(private::Private),
}

impl Interface {
    fn from_u32(raw: u32) -> Option<Self> {
        match raw {
            0 => Some(Interface::Jtag),
            1 => Some(Interface::Swd),
            _ => None,
        }
    }

    fn as_u8(self) -> u8 {
        match self {
            Interface::Jtag => 0,
            Interface::Swd => 1,
            Interface::__NonExhaustive(_) => unreachable!(),
        }
    }
}

impl fmt::Display for Interface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Interface::Jtag => f.write_str("JTAG"),
            Interface::Swd => f.write_str("SWD"),
            Interface::__NonExhaustive(_) => unreachable!(),
        }
    }
}

bitflags! {
    /// Bitset of supported target interfaces.
    pub struct Interfaces: u32 {
        /// JTAG interface.
        const JTAG = (1 << 0);
        /// SWD interface (Serial Wire Debug).
        const SWD = (1 << 1);
    }
}

impl Interfaces {
    /// Returns an iterator over all [`Interface`]s in this bitset.
    ///
    /// [`Interface`]: enum.Interface.html
    pub fn into_iter(self) -> impl Iterator<Item = Interface> {
        [(Self::JTAG, Interface::Jtag), (Self::SWD, Interface::Swd)]
            .iter()
            .filter(move |(flag, _)| self.contains(*flag))
            .map(|(_, intf)| *intf)
    }
}

/// A hardware version returned by [`JayLink::read_hardware_version`].
///
/// Note that the reported hardware version does not allow reliable feature detection, since
/// embedded J-Link probes might return a hardware version of 1.0.0 despite supporting SWD and other
/// much newer features.
///
/// [`JayLink::read_hardware_version`]: struct.JayLink.html#method.read_hardware_version
#[derive(Debug)]
pub struct HardwareVersion(u32);

impl HardwareVersion {
    fn from_u32(raw: u32) -> Self {
        HardwareVersion(raw)
    }

    /// Returns the type of hardware (or `None` if the hardware type is unknown).
    pub fn hardware_type(&self) -> Option<HardwareType> {
        Some(match (self.0 / 1000000) % 100 {
            0 => HardwareType::JLink,
            1 => HardwareType::JTrace,
            2 => HardwareType::Flasher,
            3 => HardwareType::JLinkPro,
            _ => return None,
        })
    }

    /// The major version.
    pub fn major(&self) -> u8 {
        // Decimal coded Decimal, cool cool
        (self.0 / 10000) as u8
    }

    /// The minor version.
    pub fn minor(&self) -> u8 {
        ((self.0 % 10000) / 100) as u8
    }

    /// The hardware revision.
    pub fn revision(&self) -> u8 {
        (self.0 % 100) as u8
    }
}

impl fmt::Display for HardwareVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(hw) = self.hardware_type() {
            write!(f, "{:?} ", hw)?;
        }
        write!(f, "{}.{}.{}", self.major(), self.minor(), self.revision())
    }
}

/// The hardware/product type of the device.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HardwareType {
    JLink,
    JTrace,
    Flasher,
    JLinkPro,

    #[doc(hidden)]
    __NonExhaustive(private::Private),
}

/// J-Link CPU frequency info.
#[derive(Debug)]
pub struct Speeds {
    base_freq: u32,
    min_div: u16,
}

impl Speeds {
    pub fn base_freq(&self) -> u32 {
        self.base_freq
    }

    pub fn min_div(&self) -> u16 {
        self.min_div
    }
}

/// Generic info about a USB device.
///
/// Returned by [`scan_usb`].
///
/// [`scan_usb`]: fn.scan_usb.html
#[derive(Debug)]
pub struct UsbDeviceInfo {
    inner: rusb::Device<rusb::GlobalContext>,
    vid: u16,
    pid: u16,
}

impl UsbDeviceInfo {
    /// Returns the vendor ID.
    ///
    /// Vendor IDs are centrally registered and can be looked up for example at
    /// [http://www.linux-usb.org/usb.ids](http://www.linux-usb.org/usb.ids).
    pub fn vid(&self) -> u16 {
        self.vid
    }

    /// Returns the product ID.
    pub fn pid(&self) -> u16 {
        self.pid
    }

    /// Returns the bus this device is attached to.
    pub fn bus_number(&self) -> u8 {
        self.inner.bus_number()
    }

    /// Returns the device address on the bus it's attached to.
    pub fn address(&self) -> u8 {
        self.inner.address()
    }

    /// Returns the port the device is attached to.
    pub fn port_number(&self) -> u8 {
        self.inner.port_number()
    }

    /// Tries to open this USB device.
    ///
    /// If successful, returns a [`JayLink`] instance.
    ///
    /// This method is equivalent to [`JayLink::open_usb`].
    ///
    /// [`JayLink`]: struct.JayLink.html
    /// [`JayLink::open_usb`]: struct.JayLink.html#method.open_usb
    pub fn open(self) -> Result<JayLink> {
        JayLink::open_usb(self)
    }
}

/// Scans for J-Link USB devices.
///
/// The returned iterator will yield all devices made by Segger, without filtering the product ID.
pub fn scan_usb() -> Result<impl Iterator<Item = UsbDeviceInfo>> {
    log_libusb_info();

    Ok(rusb::devices()
        .jaylink_err()?
        .iter()
        .filter_map(|dev| {
            // This calls `libusb_get_device_descriptor`, which should be unable to fail in any
            // libusb version (it only accesses cached descriptor data).
            let descr = dev
                .device_descriptor()
                .expect("libusb_get_device_descriptor returned unexpected error");

            if descr.vendor_id() == VID_SEGGER {
                Some(UsbDeviceInfo {
                    vid: descr.vendor_id(),
                    pid: descr.product_id(),
                    inner: dev,
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .into_iter())
}

fn log_libusb_info() {
    static DID_LOG: AtomicBool = AtomicBool::new(false);

    if DID_LOG.swap(true, Ordering::Acquire) {
        return;
    }

    let vers = rusb::version();
    debug!(
        "libusb {}.{}.{}.{}{}",
        vers.major(),
        vers.minor(),
        vers.micro(),
        vers.nano(),
        vers.rc()
            .map(|rc| format!("-{}", rc))
            .unwrap_or(String::new())
    );

    debug!("libusb has capability API: {:?}", rusb::has_capability());
    debug!("libusb has HID access: {:?}", rusb::has_hid_access());
    debug!("libusb has hotplug support: {:?}", rusb::has_hotplug());
    debug!(
        "libusb can detach kernel driver: {:?}",
        rusb::supports_detach_kernel_driver()
    );
}
