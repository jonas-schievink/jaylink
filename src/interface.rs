use bitflags::bitflags;
use std::fmt;

/// List of target interfaces (JTAG / SWD).
#[non_exhaustive]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Interface {
    /// JTAG interface.
    Jtag = 0,
    /// SWD interface (Serial Wire Debug).
    Swd = 1,
}

impl Interface {
    const ALL: &'static [Self] = &[Self::Jtag, Self::Swd];

    pub(crate) fn from_u32(raw: u32) -> Option<Self> {
        // Indices must match bit positions in `Interfaces`.
        match raw {
            0 => Some(Interface::Jtag),
            1 => Some(Interface::Swd),
            _ => None,
        }
    }

    pub(crate) fn as_u8(self) -> u8 {
        self as u8
    }
}

impl fmt::Display for Interface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Interface::Jtag => f.write_str("JTAG"),
            Interface::Swd => f.write_str("SWD"),
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
            Interfaces::all().into_iter().collect::<Vec<_>>(),
            &[Interface::Jtag, Interface::Swd]
        );
    }
}
