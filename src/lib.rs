//! An API for reading [Nutrimatic] index files.
//!
//! See the Nutrimatic source code for a [full description of the file
//! format][format] or [instructions for creating an index file][instructions].
//!
//! An index file is taken in as a `&[u8]` containing the contents of the file;
//! typically, this will be created by memory-mapping a file on disk (of course,
//! it also works fine to read an index file fully into memory if it fits).
//!
//! An index file describes a trie of strings; edges are labeled with characters
//! (ASCII space, digits, and letters) and each node stores the total frequency
//! in some corpus of all phrases starting with the sequence of characters
//! leading up to the node.
//!
//! This library does no consistency checking of index files. If you attempt to
//! use an invalid file, you will see random panics or garbage results (but no
//! unsafety). Don't do that!
//!
//! # Examples
//!
//! ```no_run
//! use nutrimatic::Node;
//!
//! // Print out all nodes in the trie and their frequencies.
//! fn dump(word: &mut String, node: &Node, depth: usize) {
//!     for link in &node.links() {
//!         word.push(link.ch() as char);
//!         println!("{}'{}' {:8}", "    ".repeat(depth), word, link.freq());
//!         if let Some(c) = link.child() {
//!             dump(word, &c, depth + 1);
//!         }
//!         word.pop();
//!     }
//! }
//!
//! fn main() {
//!     // Get a buffer containing an index file somehow.
//!     let buf: &[u8] = todo!();
//!     let trie = Node::new(buf);
//!     dump(&mut String::new(), &trie, 0);
//! }
//! ```
//!
//! [Nutrimatic]: https://nutrimatic.org
//! [format]: https://github.com/egnor/nutrimatic/blob/master/index.h
//! [instructions]: https://github.com/egnor/nutrimatic/blob/master/README

#![warn(missing_docs)]

use std::cmp::Ordering;

mod node_types;

/// A link leading out of a node in the trie.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Link<'buf> {
    ch: u8,
    freq: u64,

    buf: &'buf [u8],
    loc: Option<usize>,
}

impl<'buf> Link<'buf> {
    /// Returns the child node pointed to by this link, if there is one. Leaf
    /// nodes have a link notionally leading to them but are not physically
    /// represented in the index file; they correspond to a return value of
    /// `None`.
    pub fn child(&self) -> Option<Node<'buf>> {
        self.loc.map(|l| Node {
            buf: self.buf,
            loc: l,
            freq: self.freq,
        })
    }

    /// Returns the character associated with the link.
    pub fn ch(&self) -> u8 {
        self.ch
    }

    /// Returns the frequency of the link—i.e., the total number of times that
    /// the corpus contains any phrase that starts with the sequence of
    /// characters corresponding to the path to this link (including this link's
    /// own character).
    pub fn freq(&self) -> u64 {
        self.freq
    }
}

/// An iterator over the links leading out of a node in the trie.
///
/// This `struct` is created by iterating over [`LinkReader`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

/// A lazy reader of the links leading out of a node in the trie.
///
/// This `struct` is created by the [`links`] method on [`Node`]. It
/// conceptually represents a sequence of [`Link`]s indexed by contiguous
/// numbers, kind of like `[Link]`; however, it does not physically contain any
/// instances, instead constructing them on demand.
///
/// # Examples
///
/// ```no_run
/// use nutrimatic::{Link, Node};
///
/// // Materialize all the links leading out of a node.
/// fn collect_links<'a>(node: &Node<'a>) -> Vec<Link<'a>> {
///     node.links().iter().collect()
/// }
/// ```
///
/// [`links`]: Node::links
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LinkReader<'buf> {
    num: usize,
    buf: &'buf [u8],
    base: usize,
    freq: u64,
    read_fn: fn(&'buf [u8], usize, usize, u64) -> Link<'buf>,
}

impl<'buf> LinkReader<'buf> {
    /// Returns the link at the given index, starting from 0. Links are in
    /// increasing order by character. If the index is not within bounds, this
    /// method will panic or return garbage.
    pub fn index(&self, index: usize) -> Link<'buf> {
        (self.read_fn)(self.buf, self.base, index, self.freq)
    }

    /// Returns the number of links in this sequence.
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
    pub fn scan(&self, ch: u8) -> Option<Link<'buf>> {
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

/// A node in the trie that is physically present in an index file.
///
/// The unit of serialization in an index file, and what we mean by "node", is a
/// sequence of links to children. Therefore, leaf nodes in the conceptual trie
/// are not physically present in the file and have no representation as
/// `Node`s.
///
/// ```
/// use nutrimatic::Node;
///
/// // This buffer represents a single node with three links to leaf children.
/// let buf: &[u8] = &[0x61, 0x11, 0x62, 0x12, 0x63, 0x13, 0x03];
///
/// let trie = Node::new(buf);
/// // Print out all links from the root node of the trie.
/// for link in &trie.links() {
///     println!("{} {}", link.ch() as char, link.freq());
/// }
/// ```
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Node<'buf> {
    buf: &'buf [u8],
    loc: usize,
    freq: u64,
}

/// The result of searching for a string.
///
/// This `struct` is created by the [`search_string`] method on [`Node`]. See
/// its documentation for more.
///
/// [`search_string`]: Node::search_string
pub enum SearchResult<'buf> {
    /// Result indicating that no link was found after a certain number of
    /// characters.
    FailedOn(usize),
    /// Result indicating that the string was found with the given frequency and
    /// following links.
    Found {
        /// The frequency.
        freq: u64,
        /// The following links.
        links: Option<LinkReader<'buf>>,
    },
}

impl<'buf> Node<'buf> {
    /// Constructs the root node of the trie serialized in the given buffer.
    pub fn new(buf: &'buf [u8]) -> Node<'buf> {
        Node {
            buf,
            loc: buf.len(),
            freq: 0,
        }
    }

    /// Parses a node and returns an object representing the sequence of links
    /// leading out from it.
    pub fn links(self: &Self) -> LinkReader<'buf> {
        let ind = self.loc - 1;
        let sig = self.buf[ind];
        let (sig_base, elem_bytes, func): (_, _, fn(&[u8], usize, usize, u64) -> Link) = match sig {
            0x20..=0x7f => {
                return LinkReader {
                    num: 1,
                    buf: self.buf,
                    base: ind,
                    freq: self.freq,
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

    /// Searches multiple levels through the trie in one call.
    pub fn search_string(self: &Self, q: &[u8]) -> SearchResult<'buf> {
        let mut node = *self;
        let mut links = self.links();

        for (i, &ch) in q.iter().enumerate() {
            match links.find(ch) {
                None => {
                    return SearchResult::FailedOn(i);
                }
                Some(link) => match link.child() {
                    None => {
                        return if i == q.len() - 1 {
                            SearchResult::Found {
                                freq: link.freq,
                                links: None,
                            }
                        } else {
                            SearchResult::FailedOn(i + 1)
                        };
                    }
                    Some(c) => {
                        node = c;
                        links = node.links();
                    }
                },
            }
        }
        SearchResult::Found {
            freq: node.freq,
            links: Some(links),
        }
    }

    /// Finds the frequency of the given sequence of characters in the trie.
    ///
    /// This function performs a query for the given characters followed by a
    /// space character and returns the frequency of the final link, if found.
    pub fn word_freq(self: &Self, word: &[u8]) -> Option<u64> {
        match self.search_string(word) {
            SearchResult::FailedOn(_) => None,
            SearchResult::Found { links, .. } => {
                links.and_then(|l| l.scan(' ' as u8)).map(|l| l.freq)
            }
        }
    }
}
