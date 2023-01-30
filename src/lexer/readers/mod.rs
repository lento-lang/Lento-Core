pub mod bytes_reader;
pub mod stdin;
pub mod reset_buf_reader;

pub trait Resettable {
    fn reset(&mut self);
}
