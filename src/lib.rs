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

mod node_types;

/// An opaque object that can be passed to [`read_node`] to query the child node
/// at the end of a link in the trie.
///
/// [`read_node`]: Trie::read_node
#[derive(Clone, Copy, Debug)]
pub struct Cursor<'buf> {
    freq: usize,
    loc: usize,
    _phantom: std::marker::PhantomData<&'buf [u8]>,
}

/// A link from a node in the trie to one of its children.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Link<'buf> {
    /// The character associated with the link.
    pub ch: u8,
    /// The frequency of the child node.
    pub freq: usize,
    loc: Option<usize>,

    _phantom: std::marker::PhantomData<&'buf [u8]>,
}

impl<'buf> Link<'buf> {
    /// Constructs a cursor that can be used to query for the child node at the
    /// end of this link using [`read_node`] (returns [`None`](None) if the
    /// child is a leaf and is therefore not actually physically represented in
    /// the file).
    ///
    /// [`read_node`]: Trie::read_node
    pub fn cursor(&self) -> Option<Cursor<'buf>> {
        self.loc.map(|l| Cursor {
            freq: self.freq,
            loc: l,
            _phantom: std::marker::PhantomData,
        })
    }
}

/// An iterator over the children of a node in the trie.
pub struct LinkIter<'buf, 'reader> {
    links: &'reader LinkReader<'buf>,
    ind: usize,
}

impl<'buf, 'reader> Iterator for LinkIter<'buf, 'reader> {
    type Item = Link<'buf>;
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
pub struct LinkReader<'buf> {
    num: usize,
    buf: &'buf [u8],
    base: usize,
    freq: usize,
    read_fn: fn(&[u8], usize, usize, usize) -> Link,
}

impl<'buf> LinkReader<'buf> {
    /// Returns the link at the given index, starting from 0. Links are in
    /// increasing order by character. If the index is not within bounds, this
    /// method will panic or return garbage.
    pub fn index(&self, index: usize) -> Link<'buf> {
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
    pub fn find(&self, ch: u8) -> Option<Link<'buf>> {
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
    pub fn iter<'reader>(&'reader self) -> LinkIter<'buf, 'reader> {
        self.into_iter()
    }
}

impl<'buf, 'reader> IntoIterator for &'reader LinkReader<'buf> {
    type Item = Link<'buf>;
    type IntoIter = LinkIter<'buf, 'reader>;
    fn into_iter(self) -> LinkIter<'buf, 'reader> {
        LinkIter {
            links: self,
            ind: 0,
        }
    }
}

/// A full Nutrimatic trie as represented in an index file.
/// ```
/// let buf: &[u8] = todo!();
/// let trie = Trie::new(buf);
/// // Print out all links from the root node of the trie.
/// for link in trie.read_node(trie.root()) {
///     println!("{:?}", link);
/// }
/// ```
#[derive(Debug)]
pub struct Trie<'buf> {
    buf: &'buf [u8],
}

#[derive(Debug)]
pub enum SearchResult<'buf> {
    FailedOn(usize),
    Found {
        freq: usize,
        cursor: Option<Cursor<'buf>>,
        links: Vec<Link<'buf>>,
    },
}

impl<'buf> Trie<'buf> {
    /// Constructs a new trie wrapping the given buffer.
    pub fn new(buf: &[u8]) -> Trie {
        Trie { buf }
    }

    /// Parses the header of a node.
    pub fn read_node(self: &Self, cursor: Cursor<'buf>) -> LinkReader<'buf> {
        let ind = cursor.loc - 1;
        let sig = self.buf[ind];
        let (sig_base, elem_bytes, func): (_, _, fn(&[u8], usize, usize, usize) -> Link) = match sig
        {
            0x20..=0x7f => {
                return LinkReader {
                    num: 1,
                    buf: self.buf,
                    base: ind,
                    freq: cursor.freq,
                    read_fn: node_types::read_00,
                }
            }
            0x00..=0x1f => (0x00, 2, node_types::read_10),
            0x80..=0x9f => (0x80, 3, node_types::read_11),
            0xa0..=0xbf => (0xa0, 4, node_types::read_12),
            0xc0..=0xdf => (0xc0, 5, node_types::read_22),
            0xe0..=0xff => (0xe0, 17, node_types::read_88),
        };

        let (ind, num) = match self.buf[ind] - sig_base {
            0 => (ind - 1, self.buf[ind - 1]),
            d if d < 0x20 => (ind, d),
            _ => unreachable!(),
        };

        LinkReader {
            num: num.into(),
            buf: self.buf,
            base: ind - elem_bytes * num as usize,
            // Not actually used in this case.
            freq: 0,
            read_fn: func,
        }
    }

    /// Returns a cursor corresponding to the root node of the trie.
    pub fn root(self: &Self) -> Cursor<'buf> {
        Cursor {
            freq: 0,
            loc: self.buf.len(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn search_string(self: &Self, q: &[u8]) -> SearchResult<'buf> {
        let mut cursor = self.root();
        let mut links = self.read_node(cursor);

        for (i, &ch) in q.iter().enumerate() {
            match links.find(ch) {
                None => {
                    return SearchResult::FailedOn(i);
                }
                Some(link) => match link.cursor() {
                    None => {
                        return if i == q.len() - 1 {
                            SearchResult::Found {
                                freq: link.freq,
                                cursor: None,
                                links: vec![],
                            }
                        } else {
                            SearchResult::FailedOn(i + 1)
                        };
                    }
                    Some(c) => {
                        cursor = c;
                        links = self.read_node(cursor);
                    }
                },
            }
        }
        SearchResult::Found {
            freq: cursor.freq,
            cursor: Some(cursor),
            links: links.iter().collect(),
        }
    }

    pub fn word_freq(self: &Self, word: &[u8]) -> Option<usize> {
        match self.search_string(word) {
            SearchResult::FailedOn(_) => None,
            SearchResult::Found { links, .. } => links
                .into_iter()
                .find(|l| l.ch == ' ' as u8)
                .map(|l| l.freq),
        }
    }
}
