use byteorder::{ByteOrder, LE};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Link {
    pub ch: u8,
    pub freq: usize,
    pub loc: Option<usize>,
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
