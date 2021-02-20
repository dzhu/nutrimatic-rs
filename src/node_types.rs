use byteorder::{ByteOrder, LE};

pub(crate) fn read_00<'buf>(
    buf: &'buf [u8],
    base: usize,
    _ind: usize,
    freq: u64,
) -> crate::Node<'buf> {
    crate::Node {
        freq,
        ch: buf[base],
        buf,
        loc: Some(base),
    }
}

pub(crate) fn read_10<'buf>(
    buf: &'buf [u8],
    base: usize,
    ind: usize,
    _freq: u64,
) -> crate::Node<'buf> {
    crate::Node {
        freq: buf[base + 2 * ind + 1] as u64,
        ch: buf[base + 2 * ind],
        buf,
        loc: None,
    }
}

pub(crate) fn read_11<'buf>(
    buf: &'buf [u8],
    base: usize,
    ind: usize,
    _freq: u64,
) -> crate::Node<'buf> {
    let ofs = buf[base + 3 * ind + 2];
    crate::Node {
        freq: buf[base + 3 * ind + 1] as u64,
        ch: buf[base + 3 * ind],
        buf,
        loc: if ofs == u8::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_12<'buf>(
    buf: &'buf [u8],
    base: usize,
    ind: usize,
    _freq: u64,
) -> crate::Node<'buf> {
    let ofs = LE::read_u16(&buf[base + 4 * ind + 2..]);
    crate::Node {
        freq: buf[base + 4 * ind + 1] as u64,
        ch: buf[base + 4 * ind],
        buf,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_22<'buf>(
    buf: &'buf [u8],
    base: usize,
    ind: usize,
    _freq: u64,
) -> crate::Node<'buf> {
    let ofs = LE::read_u16(&buf[base + 5 * ind + 3..]);
    crate::Node {
        freq: LE::read_u16(&buf[base + 5 * ind + 1..]) as u64,
        ch: buf[base + 5 * ind],
        buf,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_88<'buf>(
    buf: &'buf [u8],
    base: usize,
    ind: usize,
    _freq: u64,
) -> crate::Node<'buf> {
    let ofs = LE::read_u64(&buf[base + 17 * ind + 9..]);
    crate::Node {
        freq: LE::read_u64(&buf[base + 17 * ind + 1..]) as u64,
        ch: buf[base + 17 * ind],
        buf,
        loc: if ofs == u64::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}
