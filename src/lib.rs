//! An API for reading [Nutrimatic] index files.
//!
//! See the Nutrimatic source code for a [full description of the file
//! format][format] or [instructions for creating an index file][instructions].
//!
//! An index file is represented as a `&[u8]` containing the contents of the
//! file; typically, this will be provided by memory-mapping a file on disk.
//!
//! [Nutrimatic]: https://nutrimatic.org
//! [format]: https://github.com/egnor/nutrimatic/blob/master/index.h
//! [instructions]: https://github.com/egnor/nutrimatic/blob/master/README

use std::cmp::Ordering;

use byteorder::{ByteOrder, LE};

/// A link from a node in the trie to one of its children.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Link {
    /// The character associated with the link.
    pub ch: u8,
    /// The frequency of the child node.
    pub freq: usize,
    /// The location in the file of the child node ([`None`](None) if the child
    /// is a leaf and is therefore not actually physically represented in the
    /// file).
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

/// An iterator over the children of a node in the trie.
pub struct LinkIter<'a> {
    links: &'a LinkReader<'a>,
    ind: usize,
}

impl<'a> Iterator for LinkIter<'a> {
    type Item = Link;
    fn next(&mut self) -> Option<Self::Item> {
        if self.ind < self.links.len() {
            self.ind += 1;
            Some(self.links.index(self.ind - 1))
        } else {
            None
        }
    }
}

/// A lazy reader of the children of a node in the trie.
pub struct LinkReader<'a> {
    num: usize,
    buf: &'a [u8],
    base: usize,
    freq: usize,
    read_fn: fn(&[u8], usize, usize, usize) -> Link,
}

impl<'a> LinkReader<'a> {
    /// Returns the link at the given index, starting from 0. Links are in
    /// increasing order by character. If the index is not within bounds, this
    /// method will panic or return garbage.
    pub fn index(&self, index: usize) -> Link {
        (self.read_fn)(self.buf, self.base, index, self.freq)
    }

    /// Returns the number of links (children) that the node has.
    pub fn len(&self) -> usize {
        self.num
    }

    /// Returns `true` if the node has no children.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Finds a link with the given character using binary search.
    pub fn find(&self, ch: u8) -> Option<Link> {
        let mut a = 0;
        let mut b = self.len();
        while b > a {
            let c = (a + b) / 2;
            let link = self.index(c);
            match link.ch.cmp(&ch) {
                Ordering::Less => {
                    a = c + 1;
                }
                Ordering::Equal => {
                    return Some(link);
                }
                Ordering::Greater => {
                    b = c;
                }
            }
        }
        None
    }

    /// Finds a link with the given character by scanning through the links in
    /// order. This is useful when the character is known to be early in the
    /// list (in particular, the space character is always first if present).
    pub fn scan(&self, ch: u8) -> Option<Link> {
        for i in 0..self.len() {
            let link = self.index(i);
            match link.ch.cmp(&ch) {
                Ordering::Less => {}
                Ordering::Equal => return Some(link),
                Ordering::Greater => return None,
            }
        }
        None
    }

    /// Returns an iterator over the links.
    pub fn iter(&self) -> LinkIter<'_> {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a LinkReader<'a> {
    type Item = Link;
    type IntoIter = LinkIter<'a>;
    fn into_iter(self) -> LinkIter<'a> {
        LinkIter {
            links: self,
            ind: 0,
        }
    }
}

/// Parses the header of a node.
pub fn read_node(buf: &[u8], ind: usize, freq: usize) -> LinkReader<'_> {
    let ind = ind - 1;
    let sig = buf[ind];
    let (sig_base, elem_bytes, func): (_, _, fn(&[u8], usize, usize, usize) -> Link) = match sig {
        0x20..=0x7f => {
            return LinkReader {
                num: 1,
                buf,
                base: ind,
                freq,
                read_fn: read_node_00,
            }
        }
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

    LinkReader {
        num: num.into(),
        buf: buf,
        base: ind - elem_bytes * num as usize,
        freq,
        read_fn: func,
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
        match links.find(ch) {
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
        links: links.iter().collect(),
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
