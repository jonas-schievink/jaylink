use bitflags::bitflags;
use std::fmt;

/// List of target interfaces (JTAG / SWD).
#[non_exhaustive]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Interface {
    /// JTAG interface.
    Jtag = 0,

    /// SWD interface (Serial Wire Debug), used by most Cortex-M chips.
    Swd = 1,

    /// Background Debug Mode 3.
    Bdm3 = 2,

    /// FINE, a two-wire debugging interface used by Renesas RX MCUs.
    Fine = 3,

    /// In-Circuit System Programming (ICSP) interface of PIC32 chips.
    Pic32Icsp = 4,

    /// Serial Peripheral Interface (for SPI Flash programming).
    Spi = 5,

    /// Silicon Labs' 2-wire debug interface.
    C2 = 6,

    /// [cJTAG], or compact JTAG, as specified in IEEE 1149.7.
    ///
    /// [cJTAG]: https://wiki.segger.com/J-Link_cJTAG_specifics.
    CJtag = 7,

    /// 2-wire debugging interface used by Microchip's IS208x MCUs.
    Mc2WireJtag = 10,
    // (*)
    // NOTE: When changing this enum, also change all other places with a (*) in addition to
    // anything that fails to compile.
    // NOTE 2: Keep the docs in sync with the bitflags below!
}

impl Interface {
    const ALL: &'static [Self] = &[
        Self::Jtag,
        Self::Swd,
        Self::Bdm3,
        Self::Fine,
        Self::Pic32Icsp,
        Self::Spi,
        Self::C2,
        Self::CJtag,
        Self::Mc2WireJtag,
        // (*)
    ];

    pub(crate) fn from_u32(raw: u32) -> Option<Self> {
        // Indices must match bit positions in `Interfaces`.
        match raw {
            0 => Some(Interface::Jtag),
            1 => Some(Interface::Swd),
            2 => Some(Interface::Bdm3),
            3 => Some(Interface::Fine),
            4 => Some(Interface::Pic32Icsp),
            5 => Some(Interface::Spi),
            6 => Some(Interface::C2),
            7 => Some(Interface::CJtag),
            10 => Some(Interface::Mc2WireJtag),
            // (*)
            _ => None,
        }
    }

    pub(crate) fn as_u8(self) -> u8 {
        self as u8
    }
}

impl fmt::Display for Interface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Interface::Jtag => "JTAG",
            Interface::Swd => "SWD",
            Interface::Bdm3 => "BDM3",
            Interface::Fine => "FINE",
            Interface::Pic32Icsp => "PIC32 ICSP",
            Interface::Spi => "SPI",
            Interface::C2 => "C2",
            Interface::CJtag => "cJTAG",
            Interface::Mc2WireJtag => "Microchip 2-wire JTAG",
        })
    }
}

bitflags! {
    /// Bitset of supported target interfaces.
    pub struct Interfaces: u32 {
        /// JTAG interface.
        const JTAG = 1 << 0;

        /// SWD interface (Serial Wire Debug), used by most Cortex-M chips.
        const SWD = 1 << 1;

        /// Background Debug Mode 3.
        const BDM3 = 1 << 2;

        /// FINE, a two-wire debugging interface used by Renesas RX MCUs.
        const FINE = 1 << 3;

        /// In-Circuit System Programming (ICSP) interface of PIC32 chips.
        const PIC32_ICSP = 1 << 4;

        /// Serial Peripheral Interface.
        const SPI = 1 << 5;

        /// Silicon Labs' 2-wire debug interface.
        const C2 = 1 << 6;

        /// [cJTAG], or compact JTAG, as specified in IEEE 1149.7.
        ///
        /// [cJTAG]: https://wiki.segger.com/J-Link_cJTAG_specifics.
        const CJTAG = 1 << 7;

        /// 2-wire debugging interface used by Microchip's IS208x MCUs.
        const MC_2WIRE_JTAG = 1 << 10;
        // (*)
    }
}

impl Interfaces {
    pub(crate) fn from_bits_warn(raw: u32) -> Self {
        let this = Self::from_bits_truncate(raw);
        if this.bits() != raw {
            log::debug!(
                "unknown bits in interface mask: 0x{:08X} truncated to 0x{:08X} ({:?})",
                raw,
                this.bits(),
                this,
            );
        }
        this
    }

    /// Returns an iterator over all [`Interface`]s in this bitset.
    ///
    /// [`Interface`]: enum.Interface.html
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> InterfaceIter {
        // FIXME: Rename to `.iter()`
        InterfaceIter {
            interfaces: self,
            next: 0,
        }
    }
}

impl IntoIterator for Interfaces {
    type Item = Interface;
    type IntoIter = InterfaceIter;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter()
    }
}

/// Iterator over supported `Interface`s.
#[derive(Debug)]
pub struct InterfaceIter {
    interfaces: Interfaces,
    next: usize,
}

impl Iterator for InterfaceIter {
    type Item = Interface;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = Interface::ALL.get(self.next)?;
            self.next += 1;
            if self
                .interfaces
                .contains(Interfaces::from_bits(1 << next.as_u8()).unwrap())
            {
                return Some(*next);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter() {
        assert_eq!(Interfaces::empty().into_iter().collect::<Vec<_>>(), &[]);
        assert_eq!(
            Interfaces::JTAG.into_iter().collect::<Vec<_>>(),
            &[Interface::Jtag]
        );
        assert_eq!(
            Interfaces::SWD.into_iter().collect::<Vec<_>>(),
            &[Interface::Swd]
        );
        assert_eq!(
            (Interfaces::JTAG | Interfaces::SWD)
                .into_iter()
                .collect::<Vec<_>>(),
            &[Interface::Jtag, Interface::Swd]
        );
    }
}

// FIXME: make bitfields-generated struct private
