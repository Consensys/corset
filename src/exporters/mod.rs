use eyre::*;
use std::io::BufWriter;

pub mod go;
pub mod latex;

pub(crate) trait Exporter<T> {
    fn render<'a>(&mut self, cs: &[T], out: BufWriter<Box<dyn std::io::Write + 'a>>) -> Result<()>;
}
