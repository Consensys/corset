use crate::utils::ConstraintsSet;
use eyre::*;
use std::io::BufWriter;

pub mod go;

pub(crate) trait Transpiler {
    fn render<'a>(
        &self,
        cs: &ConstraintsSet,
        out: BufWriter<Box<dyn std::io::Write + 'a>>,
    ) -> Result<()>;
}
