use std::io::{BufReader, SeekFrom, Seek, BufRead, Read};

use super::Resettable;

// simple Resettable BufReader wrapper
pub struct ResetBufReader<R: Seek + Read> {
    reader: BufReader<R>
}

impl<R: Seek + Read> ResetBufReader<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader: BufReader::new(reader)
        }
    }
}

impl<R: Seek + Read> Read for ResetBufReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

impl<R: Seek + Read> BufRead for ResetBufReader<R> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        self.reader.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.reader.consume(amt);
    }
}

impl<R: Seek + Read> Seek for ResetBufReader<R> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        self.reader.seek(pos)
    }
}

impl<R: Seek + Read> Resettable for ResetBufReader<R> {
    fn reset(&mut self) {
        self.reader.seek(SeekFrom::Start(0)).unwrap();
    }
}
