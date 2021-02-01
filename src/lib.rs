#[derive(Debug)]
pub struct Link {
    pub ch: u8,
    pub freq: usize,
    pub loc: Option<usize>,
}

pub fn read_node(buf: &[u8], ind: usize, freq: usize) -> Vec<Link> {
    use byteorder::{ByteOrder, LE};
    let ind = ind - 1;

    #[derive(Clone, Copy)]
    enum ByteSize {
        B0,
        B1,
        B2,
        B8,
    }

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
