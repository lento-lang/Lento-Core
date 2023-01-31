use std::io::{Read, Seek, Stdin, BufRead};

pub struct StdinReader {
    stdin: std::io::Stdin,
    buffer: String,
    buffer_pos: usize,
    should_read: bool,
}

impl StdinReader {
    /// Wrap a string in a `StringReader`, which implements `std::io::Read`.
    pub fn new(stdin: std::io::Stdin) -> Self {
        Self {
            stdin,
            buffer: String::new(),
            buffer_pos: 0,
            should_read: true,
        }
    }

    fn reset_buffer(&mut self) {
        self.buffer.clear();
        self.buffer_pos = 0;
    }

    pub fn reset_reader(&mut self) {
        self.reset_buffer();
        self.should_read = true;
    }
}

impl Read for StdinReader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.should_read || self.buffer_pos >= self.buffer.len() {
            self.reset_buffer();
            self.stdin.read_line(&mut self.buffer)?;
            self.should_read = false;
        }
        let amt = std::cmp::min(buf.len(), self.buffer.len() - self.buffer_pos);
        buf[..amt].copy_from_slice(&self.buffer.as_bytes()[self.buffer_pos..self.buffer_pos + amt]);
        self.buffer_pos += amt;
        Ok(amt)
    }
}

impl BufRead for StdinReader {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        // Use the read method to fill the buffer if it's empty.
        let _ = self.read(&mut [])?;
        // Return the previous buffer contents or new content if the buffer was empty.
        Ok(&self.buffer.as_bytes()[self.buffer_pos..])
    }

    fn consume(&mut self, amt: usize) {
        self.buffer_pos = std::cmp::min(self.buffer_pos + amt, self.buffer.len());
    }
}

impl Seek for StdinReader {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        match pos {
            std::io::SeekFrom::Start(pos) => {
                self.buffer_pos = pos as usize;
            }
            std::io::SeekFrom::End(pos) => {
                self.buffer_pos = (self.buffer.len() as i64 - pos) as usize;
            }
            std::io::SeekFrom::Current(pos) => {
                self.buffer_pos = (self.buffer_pos as i64 + pos) as usize;
            }
        }
        // Check if the seek position is valid.
        if self.buffer_pos > self.buffer.len() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "invalid seek to a negative or overflowing position",
            ));
        }
        Ok(self.buffer_pos as u64)
    }
}

impl From<Stdin> for StdinReader {
    fn from(stdin: Stdin) -> Self {
        Self::new(stdin)
    }
}
