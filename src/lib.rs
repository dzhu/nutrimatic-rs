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
//! // Print out all phrases in the trie in alphabetical order along with their
//! // frequencies.
//! fn dump(word: &mut String, node: &Node, depth: usize) {
//!     for child in &node.children() {
//!         // The space indicates that this transition corresponds to a word
//!         // boundary.
//!         if child.ch() == ' ' as u8 {
//!             println!("{}'{}' {:8}", "    ".repeat(depth), word, child.freq());
//!         }
//!         word.push(child.ch() as char);
//!         dump(word, &child, depth + 1);
//!         word.pop();
//!     }
//! }
//!
//! fn main() {
//!     // Get a buffer containing an index file somehow.
//!     let buf: &[u8] = todo!();
//!     dump(&mut String::new(), &Node::new(buf), 0);
//! }
//! ```
//!
//! [Nutrimatic]: https://nutrimatic.org
//! [format]: https://github.com/egnor/nutrimatic/blob/master/index.h
//! [instructions]: https://github.com/egnor/nutrimatic/blob/master/README

#![warn(missing_docs)]

use std::cmp::Ordering;

mod node_types;

/// An iterator over the children of a node.
///
/// This `struct` is created by iterating over [`ChildReader`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChildIter<'buf, 'reader> {
    reader: &'reader ChildReader<'buf>,
    ind: usize,
}

impl<'buf, 'reader> Iterator for ChildIter<'buf, 'reader> {
    type Item = Node<'buf>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.ind < self.reader.len() {
            self.ind += 1;
            Some(self.reader.index(self.ind - 1))
        } else {
            None
        }
    }
}

/// A lazy reader of the children of a node.
///
/// This `struct` is created by the [`children`] method on [`Node`]. It
/// conceptually represents a sequence of [`Node`]s indexed by contiguous
/// numbers, kind of like `[Node]`; however, it does not physically contain any
/// instances, instead constructing them on demand.
///
/// # Examples
///
/// ```no_run
/// use nutrimatic::Node;
///
/// // Materialize all the children of a node into a `Vec`.
/// fn collect_children<'a>(node: &Node<'a>) -> Vec<Node<'a>> {
///     node.children().iter().collect()
/// }
/// ```
///
/// [`children`]: Node::children
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChildReader<'buf>(Option<ChildReaderInner<'buf>>);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ChildReaderInner<'buf> {
    num: usize,
    buf: &'buf [u8],
    base: usize,
    freq: u64,
    elem_bytes: u8,
    read_fn: fn(&'buf [u8], usize, usize, u64) -> Node<'buf>,
}

impl<'buf> ChildReaderInner<'buf> {
    fn index(&self, index: usize) -> Node<'buf> {
        (self.read_fn)(self.buf, self.base, index, self.freq)
    }

    fn len(&self) -> usize {
        self.num
    }

    fn find(&self, ch: u8) -> Option<Node<'buf>> {
        let mut a = 0;
        let mut b = self.len();
        while b > a {
            let c = (a + b) / 2;
            let child_ch = self.buf[self.base + c * self.elem_bytes as usize];
            match child_ch.cmp(&ch) {
                Ordering::Less => {
                    a = c + 1;
                }
                Ordering::Equal => {
                    return Some(self.index(c));
                }
                Ordering::Greater => {
                    b = c;
                }
            }
        }
        None
    }

    fn scan(&self, ch: u8) -> Option<Node<'buf>> {
        for i in 0..self.len() {
            let child_ch = self.buf[self.base + i * self.elem_bytes as usize];
            match child_ch.cmp(&ch) {
                Ordering::Less => {}
                Ordering::Equal => return Some(self.index(i)),
                Ordering::Greater => return None,
            }
        }
        None
    }
}

impl<'buf> ChildReader<'buf> {
    /// Returns the child at the given index, starting from 0. Children are in
    /// increasing order by character. If the index is not within bounds, this
    /// method will panic or return garbage.
    pub fn index(&self, index: usize) -> Node<'buf> {
        // It is incorrect to call this method if this struct contains `None`
        // (then `len() == 0`, so no index is valid), so unwrapping is okay.
        self.0.unwrap().index(index)
    }

    /// Returns the number of children of the node.
    pub fn len(&self) -> usize {
        match self.0 {
            None => 0,
            Some(inner) => inner.len(),
        }
    }

    /// Returns `true` if the node has no children.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Finds a child with the given character using binary search.
    pub fn find(&self, ch: u8) -> Option<Node<'buf>> {
        self.0.and_then(|inner| inner.find(ch))
    }

    /// Finds a child with the given character by scanning through the children
    /// in order. This is useful when the character is known to be early in the
    /// list (in particular, the space character is always first if present).
    pub fn scan(&self, ch: u8) -> Option<Node<'buf>> {
        self.0.and_then(|inner| inner.scan(ch))
    }

    /// Returns an iterator over the children.
    pub fn iter<'reader>(&'reader self) -> ChildIter<'buf, 'reader> {
        self.into_iter()
    }
}

impl<'buf, 'reader> IntoIterator for &'reader ChildReader<'buf> {
    type Item = Node<'buf>;
    type IntoIter = ChildIter<'buf, 'reader>;
    fn into_iter(self) -> ChildIter<'buf, 'reader> {
        ChildIter {
            reader: self,
            ind: 0,
        }
    }
}

/// A node in a trie.
///
/// Nodes implement ordering and equality according to their frequency in order
/// to accommodate using them in priority queues for best-first search.
///
/// # Examples
///
/// ```
/// use nutrimatic::Node;
///
/// // This buffer represents a single node with three leaf children.
/// let buf: &[u8] = &[0x61, 0x11, 0x62, 0x12, 0x63, 0x13, 0x03];
///
/// let root = Node::new(buf);
/// // Print out all children of the root node of the trie.
/// for child in &root.children() {
///     println!("{} {}", child.ch() as char, child.freq());
/// }
/// ```
#[derive(Clone, Copy, Debug)]
pub struct Node<'buf> {
    freq: u64,
    ch: u8,
    buf: &'buf [u8],
    loc: Option<usize>,
}

impl Eq for Node<'_> {}

impl Ord for Node<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.freq.cmp(&other.freq)
    }
}

impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.freq == other.freq
    }
}

impl PartialOrd for Node<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The result of searching for a string.
///
/// This `struct` is created by the [`search_string`] method on [`Node`]. See
/// its documentation for more.
///
/// [`search_string`]: Node::search_string
pub enum SearchResult<'buf> {
    /// Result indicating that no child was found after a certain number of
    /// characters.
    FailedOn(usize),
    /// Result indicating that the string was found with the given frequency and
    /// following children.
    Found {
        /// The frequency.
        freq: u64,
        /// The following children.
        children: Option<ChildReader<'buf>>,
    },
}

impl<'buf> Node<'buf> {
    /// Constructs the root node of the trie serialized in the given buffer.
    pub fn new(buf: &'buf [u8]) -> Node<'buf> {
        let mut node = Node {
            freq: 0,
            ch: 0,
            buf,
            loc: Some(buf.len()),
        };
        node.freq = node.children().iter().map(|c| c.freq()).sum();
        node
    }

    /// Returns the character associated with the node—i.e., the letter used to
    /// transition from this node's parent to this node.
    ///
    /// This value is not useful for a root node returned by [`Node::new`].
    pub fn ch(&self) -> u8 {
        self.ch
    }

    /// Returns the frequency of the node—i.e., the total number of times that
    /// the corpus contains any phrase that starts with the sequence of
    /// characters corresponding to the path to this node (including this node's
    /// own character).
    pub fn freq(&self) -> u64 {
        self.freq
    }

    /// Parses a node and returns an object representing the sequence of its
    /// children.
    pub fn children(&self) -> ChildReader<'buf> {
        let ind = match self.loc {
            Some(l) => l - 1,
            // We could return a dummy object with length 0 here, which would
            // simplify the code a bit, but it turns out that just constructing
            // and moving that out results in a huge hit to overall performance.
            None => return ChildReader(None),
        };
        let sig = self.buf[ind];
        let (sig_base, elem_bytes, func): (_, _, fn(&[u8], usize, usize, u64) -> Node) = match sig {
            0x20..=0x7f => {
                return ChildReader(Some(ChildReaderInner {
                    num: 1,
                    buf: self.buf,
                    base: ind,
                    freq: self.freq,
                    elem_bytes: 0,
                    read_fn: node_types::read_00,
                }))
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

        ChildReader(Some(ChildReaderInner {
            num: num.into(),
            buf: self.buf,
            base: ind - elem_bytes * num as usize,
            // Not actually used in this case.
            freq: 0,
            elem_bytes: elem_bytes as u8,
            read_fn: func,
        }))
    }

    /// Searches multiple levels through the trie in one call.
    pub fn search_string(&self, q: &[u8]) -> SearchResult<'buf> {
        let mut node = *self;
        let mut children = self.children();

        for (i, &ch) in q.iter().enumerate() {
            if let Some(child) = children.find(ch) {
                node = child;
                children = child.children();
            } else {
                return SearchResult::FailedOn(i);
            }
        }
        SearchResult::Found {
            freq: node.freq,
            children: Some(children),
        }
    }

    /// Finds the frequency of the given sequence of characters in the trie.
    ///
    /// This function performs a query for the given characters followed by a
    /// space character and returns the frequency of the final node, if found.
    pub fn word_freq(&self, word: &[u8]) -> Option<u64> {
        match self.search_string(word) {
            SearchResult::FailedOn(_) => None,
            SearchResult::Found { children, .. } => {
                children.and_then(|l| l.scan(' ' as u8)).map(|l| l.freq)
            }
        }
    }
}
