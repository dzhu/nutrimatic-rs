use byteorder::{ByteOrder, LE};

pub(crate) fn read_00(buf: &[u8], base: usize, _ind: usize, freq: usize) -> crate::Link {
    crate::Link {
        ch: buf[base],
        freq,
        loc: Some(base),
    }
}

pub(crate) fn read_10(buf: &[u8], base: usize, ind: usize, _freq: usize) -> crate::Link {
    crate::Link {
        ch: buf[base + 2 * ind],
        freq: buf[base + 2 * ind + 1] as usize,
        loc: None,
    }
}

pub(crate) fn read_11(buf: &[u8], base: usize, ind: usize, _freq: usize) -> crate::Link {
    let ofs = buf[base + 3 * ind + 2];
    crate::Link {
        ch: buf[base + 3 * ind],
        freq: buf[base + 3 * ind + 1] as usize,
        loc: if ofs == u8::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_12(buf: &[u8], base: usize, ind: usize, _freq: usize) -> crate::Link {
    let ofs = LE::read_u16(&buf[base + 4 * ind + 2..]);
    crate::Link {
        ch: buf[base + 4 * ind],
        freq: buf[base + 4 * ind + 1] as usize,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_22(buf: &[u8], base: usize, ind: usize, _freq: usize) -> crate::Link {
    let ofs = LE::read_u16(&buf[base + 5 * ind + 3..]);
    crate::Link {
        ch: buf[base + 5 * ind],
        freq: LE::read_u16(&buf[base + 5 * ind + 1..]) as usize,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub(crate) fn read_88(buf: &[u8], base: usize, ind: usize, _freq: usize) -> crate::Link {
    let ofs = LE::read_u64(&buf[base + 17 * ind + 9..]);
    crate::Link {
        ch: buf[base + 17 * ind],
        freq: LE::read_u64(&buf[base + 17 * ind + 1..]) as usize,
        loc: if ofs == u64::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}