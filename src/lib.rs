use byteorder::{ByteOrder, LE};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Link {
    pub ch: u8,
    pub freq: usize,
    pub loc: Option<usize>,
}

#[derive(Clone, Copy)]
enum ByteSize {
    B0,
    B1,
    B2,
    B8,
}

fn num_bytes(b: ByteSize) -> usize {
    match b {
        ByteSize::B0 => 0,
        ByteSize::B1 => 1,
        ByteSize::B2 => 2,
        ByteSize::B8 => 8,
    }
}

fn read_num(buf: &[u8], ind: usize, sz: ByteSize) -> usize {
    match sz {
        ByteSize::B0 => 0,
        ByteSize::B1 => buf[ind] as usize,
        ByteSize::B2 => LE::read_u16(&buf[ind..]) as usize,
        ByteSize::B8 => LE::read_u64(&buf[ind..]) as usize,
    }
}

fn get_ofs(buf: &[u8], base: usize, ind: usize, sz: ByteSize) -> Option<usize> {
    let check_max = |x: usize, max: usize| -> Option<usize> {
        if x == max {
            None
        } else {
            Some(x)
        }
    };

    (match sz {
        ByteSize::B0 => None,
        ByteSize::B1 => check_max(buf[ind] as usize, u8::MAX as usize),
        ByteSize::B2 => check_max(LE::read_u16(&buf[ind..]) as usize, u16::MAX as usize),
        ByteSize::B8 => check_max(LE::read_u64(&buf[ind..]) as usize, u64::MAX as usize),
    })
    .map(|ofs| base - ofs)
}

pub fn read_node(buf: &[u8], ind: usize, freq: usize) -> Vec<Link> {
    let ind = ind - 1;

    let get_ofs = |base: usize, ind: usize, sz: ByteSize| {
        let check_max = |x: usize, max: usize| -> Option<usize> {
            if x == max {
                None
            } else {
                Some(x)
            }
        };

        (match sz {
            ByteSize::B0 => None,
            ByteSize::B1 => check_max(buf[ind] as usize, u8::MAX as usize),
            ByteSize::B2 => check_max(LE::read_u16(&buf[ind..]) as usize, u16::MAX as usize),
            ByteSize::B8 => check_max(LE::read_u64(&buf[ind..]) as usize, u64::MAX as usize),
        })
        .map(|ofs| base - ofs)
    };

    let read_num = |ind: usize, sz: ByteSize| match sz {
        ByteSize::B0 => 0,
        ByteSize::B1 => buf[ind] as usize,
        ByteSize::B2 => LE::read_u16(&buf[ind..]) as usize,
        ByteSize::B8 => LE::read_u64(&buf[ind..]) as usize,
    };

    let num_bytes = |b| match b {
        ByteSize::B0 => 0,
        ByteSize::B1 => 1,
        ByteSize::B2 => 2,
        ByteSize::B8 => 8,
    };

    let do_read =
        |ind: usize, sig_base: u8, freq_size: ByteSize, ofs_size: ByteSize| -> Vec<Link> {
            let (ind, num) = match buf[ind] - sig_base {
                0 => (ind - 1, buf[ind - 1]),
                d if d < 0x20 => (ind, d),
                _ => unreachable!(),
            };

            let freq_bytes = num_bytes(freq_size);
            let ofs_bytes = num_bytes(ofs_size);
            let elem_size = 1 + freq_bytes + ofs_bytes;

            let base = ind - elem_size * num as usize;
            (0..num)
                .map(|i| Link {
                    ch: buf[base + elem_size * i as usize],
                    freq: read_num(base + elem_size * i as usize + 1, freq_size),
                    loc: get_ofs(
                        base,
                        base + elem_size * i as usize + 1 + freq_bytes,
                        ofs_size,
                    ),
                })
                .collect()
        };

    let sig = buf[ind];
    match sig {
        0x00..=0x1f => do_read(ind, 0x00, ByteSize::B1, ByteSize::B0),
        0x20..=0x7f => {
            vec![Link {
                ch: sig,
                freq: freq,
                loc: Some(ind),
            }]
        }
        0x80..=0x9f => do_read(ind, 0x80, ByteSize::B1, ByteSize::B1),
        0xa0..=0xbf => do_read(ind, 0xa0, ByteSize::B1, ByteSize::B2),
        0xc0..=0xdf => do_read(ind, 0xc0, ByteSize::B2, ByteSize::B2),
        0xe0..=0xff => do_read(ind, 0xe0, ByteSize::B8, ByteSize::B8),
    }
}

pub struct LinkReader<'buf> {
    buf: &'buf [u8],

    num: u8,

    freq_size: ByteSize,
    ofs_size: ByteSize,

    base: usize,
    freq_bytes: usize,
    elem_bytes: usize,

    single_link: Option<Link>,
}

impl<'a> LinkReader<'a> {
    fn new(buf: &'a [u8], ind: usize, freq: usize) -> LinkReader<'a> {
        let ind = ind - 1;
        let sig = buf[ind];
        let (sig_base, freq_size, ofs_size, single_link) = match sig {
            0x00..=0x1f => (0x00, ByteSize::B1, ByteSize::B0, None),
            0x20..=0x7f => (
                0x00,
                ByteSize::B0,
                ByteSize::B0,
                Some(Link {
                    ch: sig,
                    freq: freq,
                    loc: Some(ind),
                }),
            ),
            0x80..=0x9f => (0x80, ByteSize::B1, ByteSize::B1, None),
            0xa0..=0xbf => (0xa0, ByteSize::B1, ByteSize::B2, None),
            0xc0..=0xdf => (0xc0, ByteSize::B2, ByteSize::B2, None),
            0xe0..=0xff => (0xe0, ByteSize::B8, ByteSize::B8, None),
        };

        let (ind, num) = match single_link {
            Some(_) => (ind, 1),
            None => match buf[ind] - sig_base {
                0 => (ind - 1, buf[ind - 1]),
                d if d < 0x20 => (ind, d),
                _ => unreachable!(),
            },
        };

        let ofs_bytes = num_bytes(ofs_size);
        let freq_bytes = num_bytes(freq_size);
        let elem_bytes = 1 + ofs_bytes + freq_bytes;
        let base = ind - elem_bytes * num as usize;
        LinkReader {
            buf,

            num,

            freq_size,
            ofs_size,

            base,
            freq_bytes,
            elem_bytes,

            single_link,
        }
    }

    pub fn index(&self, index: usize) -> Link {
        match self.single_link {
            Some(l) => l,
            None => Link {
                ch: self.buf[self.base + self.elem_bytes as usize * index],
                freq: read_num(
                    self.buf,
                    self.base + self.elem_bytes as usize * index + 1,
                    self.freq_size,
                ),
                loc: get_ofs(
                    self.buf,
                    self.base,
                    self.base + self.elem_bytes * index + 1 + self.freq_bytes,
                    self.ofs_size,
                ),
            },
        }
    }

    pub fn len(&self) -> usize {
        self.num as usize
    }
}

pub fn read_node2(buf: &[u8], ind: usize, freq: usize) -> LinkReader {
    LinkReader::new(buf, ind, freq)
}

fn read_node_00(buf: &[u8], base: usize, _ind: usize, freq: usize) -> Link {
    Link {
        ch: buf[base],
        freq: freq,
        loc: Some(base),
    }
}

fn read_node_10(buf: &[u8], base: usize, ind: usize, _freq: usize) -> Link {
    Link {
        ch: buf[base + 2 * ind],
        freq: buf[base + 2 * ind + 1] as usize,
        loc: None,
    }
}

fn read_node_11(buf: &[u8], base: usize, ind: usize, _freq: usize) -> Link {
    let ofs = buf[base + 3 * ind + 2];
    Link {
        ch: buf[base + 3 * ind],
        freq: buf[base + 3 * ind + 1] as usize,
        loc: if ofs == u8::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

fn read_node_12(buf: &[u8], base: usize, ind: usize, _freq: usize) -> Link {
    let ofs = LE::read_u16(&buf[base + 4 * ind + 2..]);
    Link {
        ch: buf[base + 4 * ind],
        freq: buf[base + 4 * ind + 1] as usize,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

fn read_node_22(buf: &[u8], base: usize, ind: usize, _freq: usize) -> Link {
    let ofs = LE::read_u16(&buf[base + 5 * ind + 3..]);
    Link {
        ch: buf[base + 5 * ind],
        freq: LE::read_u16(&buf[base + 5 * ind + 1..]) as usize,
        loc: if ofs == u16::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

fn read_node_88(buf: &[u8], base: usize, ind: usize, _freq: usize) -> Link {
    let ofs = LE::read_u64(&buf[base + 17 * ind + 9..]);
    Link {
        ch: buf[base + 17 * ind],
        freq: LE::read_u64(&buf[base + 17 * ind + 1..]) as usize,
        loc: if ofs == u64::MAX {
            None
        } else {
            Some(base - ofs as usize)
        },
    }
}

pub fn read_node3(
    buf: &[u8],
    ind: usize,
) -> (usize, usize, fn(&[u8], usize, usize, usize) -> Link) {
    let ind = ind - 1;
    let sig = buf[ind];
    let (sig_base, elem_bytes, func): (_, _, fn(&[u8], usize, usize, usize) -> Link) = match sig {
        0x20..=0x7f => return (1, ind, read_node_00),
        0x00..=0x1f => (0x00, 2, read_node_10),
        0x80..=0x9f => (0x80, 3, read_node_11),
        0xa0..=0xbf => (0xa0, 4, read_node_12),
        0xc0..=0xdf => (0xc0, 5, read_node_22),
        0xe0..=0xff => (0xe0, 17, read_node_88),
    };

    let (ind, num) = match buf[ind] - sig_base {
        0 => (ind - 1, buf[ind - 1]),
        d if d < 0x20 => (ind, d),
        _ => unreachable!(),
    };

    let base = ind - elem_bytes * num as usize;
    (num.into(), base, func)
}

pub fn find_link(buf: &[u8], ind: usize, freq: usize, ch: u8) -> Option<Link> {
    let mut links = read_node(buf, ind, freq);
    match links.binary_search_by_key(&ch, |link| link.ch) {
        Ok(ind) => Some(links.swap_remove(ind)),
        Err(_) => None,
    }
}

#[derive(Debug)]
pub enum SearchResult {
    FailedOn(usize),
    Found {
        freq: usize,
        loc: Option<usize>,
        links: Vec<Link>,
    },
}

pub fn search_string(buf: &[u8], q: &[u8]) -> SearchResult {
    let mut loc = buf.len();
    let mut freq = 0;
    let mut links = read_node(buf, loc, freq);
    for (i, &ch) in q.iter().enumerate() {
        match links.iter().find(|link| link.ch == ch) {
            None => {
                return SearchResult::FailedOn(i);
            }
            Some(link) => match link.loc {
                None => {
                    return if i == q.len() - 1 {
                        SearchResult::Found {
                            freq: freq,
                            loc: None,
                            links: vec![],
                        }
                    } else {
                        SearchResult::FailedOn(i + 1)
                    };
                }
                Some(l) => {
                    loc = l;
                    freq = link.freq;
                    links = read_node(buf, loc, freq);
                }
            },
        }
    }
    SearchResult::Found {
        freq: freq,
        loc: Some(loc),
        links: links,
    }
}

pub fn word_freq(buf: &[u8], word: &[u8]) -> Option<usize> {
    match search_string(buf, word) {
        SearchResult::FailedOn(_) => None,
        SearchResult::Found { links, .. } => links
            .into_iter()
            .find(|l| l.ch == ' ' as u8)
            .map(|l| l.freq),
    }
}
