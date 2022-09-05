use eyre::*;
use std::io::BufWriter;

pub mod go;
pub mod latex;

pub(crate) trait Transpiler<T> {
    fn render<'a>(&self, cs: &[T], out: BufWriter<Box<dyn std::io::Write + 'a>>) -> Result<()>;
}
