#[derive(Debug)]
pub struct Link {
    pub ch: u8,
    pub freq: usize,
    pub loc: Option<usize>,
}

enum ByteSize {
    B1,
    B2,
    B8,
}

fn extend(x: usize, max: usize) -> Option<usize> {
    if x == max {
        None
    } else {
        Some(x)
    }
}

pub fn read_node(buf: &[u8], mut ind: usize, freq: usize) -> Vec<Link> {
    use byteorder::{ByteOrder, LE};
    ind -= 1;
    let sig = buf[ind];

    let get_ofs = |base: usize, ind: usize, sz: ByteSize| {
        (match sz {
            ByteSize::B1 => extend(buf[ind] as usize, u8::MAX as usize),
            ByteSize::B2 => extend(LE::read_u16(&buf[ind..]) as usize, u16::MAX as usize),
            ByteSize::B8 => extend(LE::read_u64(&buf[ind..]) as usize, u64::MAX as usize),
        })
        .map(|ofs| base - ofs)
    };

    let ret = match sig {
        0x00..=0x1f => {
            let num = if sig == 0x00 {
                ind -= 1;
                buf[ind]
            } else {
                sig
            };
            let base = ind - 2 * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + 2 * i as usize],
                    freq: buf[base + 2 * i as usize + 1] as usize,
                    loc: None,
                })
                .collect()
        }
        0x20..=0x7f => {
            vec![Link {
                ch: sig,
                freq: freq,
                loc: Some(ind),
            }]
        }
        0x80..=0x9f => {
            let num = if sig == 0x80 {
                ind -= 1;
                buf[ind]
            } else {
                sig - 0x80
            };
            let base = ind - 3 * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + 3 * i as usize],
                    freq: buf[base + 3 * i as usize + 1] as usize,
                    loc: get_ofs(base, base + 3 * i as usize + 2, ByteSize::B1),
                })
                .collect()
        }
        0xa0..=0xbf => {
            let num = if sig == 0xa0 {
                ind -= 1;
                buf[ind]
            } else {
                sig - 0xa0
            };
            let base = ind - 4 * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + 4 * i as usize],
                    freq: buf[base + 4 * i as usize + 1] as usize,
                    loc: get_ofs(base, base + 4 * i as usize + 2, ByteSize::B2),
                })
                .collect()
        }
        0xc0..=0xdf => {
            let num = if sig == 0xc0 {
                ind -= 1;
                buf[ind]
            } else {
                sig - 0xc0
            };
            let base = ind - 5 * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + 5 * i as usize],
                    freq: LE::read_u16(&buf[base + 5 * i as usize + 1..]) as usize,
                    loc: get_ofs(base, base + 5 * i as usize + 3, ByteSize::B2),
                })
                .collect()
        }
        0xe0..=0xff => {
            let num = if sig == 0xe0 {
                ind -= 1;
                buf[ind]
            } else {
                sig - 0xe0
            };
            let base = ind - 17 * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + 17 * i as usize],
                    freq: LE::read_u64(&buf[base + 17 * i as usize + 1..]) as usize,
                    loc: get_ofs(base, base + 17 * i as usize + 9, ByteSize::B8),
                })
                .collect()
        }
    };
    ret
}
