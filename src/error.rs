use std::fmt;

type BoxedError = Box<dyn std::error::Error + Send + Sync>;

/// List of specific errors that may occur when using this library.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// A USB transport error occurred.
    ///
    /// This variant is used for all errors reported by the operating system when performing a USB
    /// operation. It may indicate that the USB device was unplugged, that another application or an
    /// operating system driver is currently using it, or that the current user does not have
    /// permission to access it.
    Usb,

    /// No (matching) J-Link device was found.
    ///
    /// This error occurs when calling [`JayLink::open_by_serial`] while no J-Link device is connected
    /// (or no device matching the serial number is connected).
    ///
    /// [`JayLink::open_by_serial`]: struct.JayLink.html#method.open_by_serial
    DeviceNotFound,

    /// Automatic device connection failed because multiple devices were found.
    ///
    /// This error occurs when calling [`JayLink::open_by_serial`] without a serial number while
    /// multiple J-Link devices are connected. This library will refuse to "guess" a device and
    /// requires specifying a serial number in this case. The [`scan_usb`] function can also be used
    /// to find a specific device to connect to.
    ///
    /// [`JayLink::open_by_serial`]: struct.JayLink.html#method.open_by_serial
    /// [`scan_usb`]: fn.scan_usb.html
    MultipleDevicesFound,

    /// A operation was attempted that is not supported by the probe.
    ///
    /// Some operations are not supported by all firmware/hardware versions, and are instead
    /// advertised as optional *capability* bits. This error occurs when the capability bit for an
    /// operation isn't set when that operation is attempted.
    ///
    /// Capabilities can be read by calling [`JayLink::read_capabilities`], which returns a
    /// [`Capabilities`] bitflags struct.
    ///
    /// [`JayLink::read_capabilities`]: struct.JayLink.html#method.read_capabilities
    /// [`Capabilities`]: struct.Capabilities.html
    MissingCapability,

    /// An unspecified error occurred.
    Other,

    #[doc(hidden)]
    __NonExhaustive(crate::private::Private),
}

pub(crate) trait Cause {
    const KIND: ErrorKind;
}

/// The error type used by this library.
///
/// Errors can be introspected by the user by calling [`Error::kind`] and inspecting the returned
/// [`ErrorKind`].
///
/// [`Error::kind`]: #method.kind
/// [`ErrorKind`]: enum.ErrorKind.html
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    inner: BoxedError,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind, inner: impl Into<BoxedError>) -> Self {
        Self {
            kind,
            inner: inner.into(),
        }
    }

    /// Returns the [`ErrorKind`] describing this error.
    ///
    /// [`ErrorKind`]: enum.ErrorKind.html
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Prefix foreign errors with further explanation where they're coming from
        match self.kind {
            ErrorKind::Usb => write!(f, "USB communication error: {}", self.inner),
            _ => self.inner.fmt(f),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

pub(crate) trait ResultExt<T, E> {
    fn jaylink_err(self) -> Result<T, Error>
    where
        E: Cause + Into<BoxedError>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn jaylink_err(self) -> Result<T, Error>
    where
        E: Cause + Into<BoxedError>,
    {
        self.map_err(|e| Error::new(E::KIND, e))
    }
}

macro_rules! error_mapping {
    (
        $(
            $errty:ty => $kind:ident,
        )+
    ) => {
        $(
            impl Cause for $errty {
                const KIND: ErrorKind = ErrorKind::$kind;
            }
        )+
    };
}

error_mapping! {
    rusb::Error => Usb,
    String => Other,
}
