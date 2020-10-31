use bitflags::bitflags;
use std::{fmt, ops};

bitflags! {
    /// Extended capabilities advertised by the device.
    ///
    /// FIXME the extended capabilities are actually a 256-bit value
    struct CapabilitiesBits: u128 {
        const RESERVED_0 = (1 << 0);  // Reserved, seems to be always set
        const GET_HW_VERSION = (1 << 1);
        const WRITE_DCC = (1 << 2);
        const ADAPTIVE_CLOCKING = (1 << 3);
        const READ_CONFIG = (1 << 4);
        const WRITE_CONFIG = (1 << 5);
        const TRACE = (1 << 6);
        const WRITE_MEM = (1 << 7);
        const READ_MEM = (1 << 8);
        const SPEED_INFO = (1 << 9);
        const EXEC_CODE = (1 << 10);
        const GET_MAX_BLOCK_SIZE = (1 << 11);
        const GET_HW_INFO = (1 << 12);
        const SET_KS_POWER = (1 << 13);
        const RESET_STOP_TIMED = (1 << 14);
        // 15 = Reserved, seems to never be set
        const MEASURE_RTCK_REACT = (1 << 16);
        const SELECT_IF = (1 << 17);
        const RW_MEM_ARM79 = (1 << 18);
        const GET_COUNTERS = (1 << 19);
        const READ_DCC = (1 << 20);
        const GET_CPU_CAPS = (1 << 21);
        const EXEC_CPU_CMD = (1 << 22);
        const SWO = (1 << 23);
        const WRITE_DCC_EX = (1 << 24);
        const UPDATE_FIRMWARE_EX = (1 << 25);
        const FILE_IO = (1 << 26);
        const REGISTER = (1 << 27);
        const INDICATORS = (1 << 28);
        const TEST_NET_SPEED = (1 << 29);
        const RAWTRACE = (1 << 30);
        // For the legacy capabilities, bit 31 is documented as reserved, but it must be
        // GET_CAPS_EX, since there'd be no other way to know if GET_CAPS_EX is supported.
        const GET_CAPS_EX = (1 << 31);

        // Extended capabilities

        const HW_JTAG_WRITE = (1 << 32);
        const COM = (1 << 33);
    }
}

/// Set of capabilities advertised by the device.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Capabilities {
    inner: CapabilitiesBits,
}

macro_rules! import_capabilities {
    (
        $(
            $name:ident,
        )+
    ) => {
        $(
            pub const $name: Self = Self { inner: CapabilitiesBits::$name };
        )+
    };
}

impl Capabilities {
    import_capabilities![
        GET_HW_VERSION,
        WRITE_DCC,
        ADAPTIVE_CLOCKING,
        READ_CONFIG,
        WRITE_CONFIG,
        TRACE,
        WRITE_MEM,
        READ_MEM,
        SPEED_INFO,
        EXEC_CODE,
        GET_MAX_BLOCK_SIZE,
        GET_HW_INFO,
        SET_KS_POWER,
        RESET_STOP_TIMED,
        MEASURE_RTCK_REACT,
        SELECT_IF,
        RW_MEM_ARM79,
        GET_COUNTERS,
        READ_DCC,
        GET_CPU_CAPS,
        EXEC_CPU_CMD,
        SWO,
        WRITE_DCC_EX,
        UPDATE_FIRMWARE_EX,
        FILE_IO,
        REGISTER,
        INDICATORS,
        TEST_NET_SPEED,
        RAWTRACE,
        GET_CAPS_EX,
        HW_JTAG_WRITE,
    ];

    /// Creates a `Capabilities` instance from 32 raw bits.
    pub(crate) fn from_raw_legacy(raw: u32) -> Self {
        let capabilities = CapabilitiesBits::from_bits_truncate(u128::from(raw));
        if capabilities.bits() != u128::from(raw) {
            log::debug!(
                "unknown capability bits: 0x{:08X} truncated to 0x{:08X} ({:?})",
                raw,
                capabilities.bits(),
                capabilities,
            );
        }
        Self {
            inner: capabilities,
        }
    }

    /// Creates a `Capabilities` instance from a 256-bit bitset.
    pub(crate) fn from_raw_ex(raw: [u8; 32]) -> Self {
        let mut bytes = [0; 16];
        bytes.copy_from_slice(&raw[..16]);
        let raw = u128::from_le_bytes(bytes);
        let capabilities = CapabilitiesBits::from_bits_truncate(raw);
        if capabilities.bits() != raw {
            log::debug!(
                "unknown capability bits: 0x{:08X} truncated to 0x{:08X} ({:?})",
                raw,
                capabilities.bits(),
                capabilities,
            );
        }
        Self {
            inner: capabilities,
        }
    }

    /// Determines whether `self` contains all capabilities in `other`.
    pub fn contains(&self, other: Self) -> bool {
        self.inner.contains(other.inner)
    }
}

impl fmt::Debug for Capabilities {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl ops::BitOr for Capabilities {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self {
            inner: self.inner | rhs.inner,
        }
    }
}

// FIXME: Split up into `Capabilities` bitset and `Capability` enum?
