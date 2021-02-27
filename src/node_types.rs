use byteorder::{ByteOrder, LE};

use crate::Node;

pub(crate) type ReaderFn<'buf> = fn(&'buf [u8], usize, usize, u64) -> Node<'buf>;

pub(crate) fn read_00(buf: &[u8], base: usize, _ind: usize, freq: u64) -> Node {
    Node {
        freq,
        ch: buf[base],
        buf,
        loc: base,
    }
}

pub(crate) fn read_10(buf: &[u8], base: usize, ind: usize, _freq: u64) -> Node {
    Node {
        freq: buf[base + 2 * ind + 1] as u64,
        ch: buf[base + 2 * ind],
        buf,
        loc: usize::MAX,
    }
}

pub(crate) fn read_11(buf: &[u8], base: usize, ind: usize, _freq: u64) -> Node {
    let ofs = buf[base + 3 * ind + 2];
    Node {
        freq: buf[base + 3 * ind + 1] as u64,
        ch: buf[base + 3 * ind],
        buf,
        loc: if ofs == u8::MAX {
            usize::MAX
        } else {
            base - ofs as usize
        },
    }
}

pub(crate) fn read_12(buf: &[u8], base: usize, ind: usize, _freq: u64) -> Node {
    let ofs = LE::read_u16(&buf[base + 4 * ind + 2..]);
    Node {
        freq: buf[base + 4 * ind + 1] as u64,
        ch: buf[base + 4 * ind],
        buf,
        loc: if ofs == u16::MAX {
            usize::MAX
        } else {
            base - ofs as usize
        },
    }
}

pub(crate) fn read_22(buf: &[u8], base: usize, ind: usize, _freq: u64) -> Node {
    let ofs = LE::read_u16(&buf[base + 5 * ind + 3..]);
    Node {
        freq: LE::read_u16(&buf[base + 5 * ind + 1..]) as u64,
        ch: buf[base + 5 * ind],
        buf,
        loc: if ofs == u16::MAX {
            usize::MAX
        } else {
            base - ofs as usize
        },
    }
}

pub(crate) fn read_88(buf: &[u8], base: usize, ind: usize, _freq: u64) -> Node {
    let ofs = LE::read_u64(&buf[base + 17 * ind + 9..]);
    Node {
        freq: LE::read_u64(&buf[base + 17 * ind + 1..]) as u64,
        ch: buf[base + 17 * ind],
        buf,
        loc: if ofs == u64::MAX {
            usize::MAX
        } else {
            base - ofs as usize
        },
    }
}
