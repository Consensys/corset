use crate::{
    column::ValueBacking,
    compiler::{ColumnRef, ConstraintSet, Kind},
    pretty::{Base, Pretty},
};
use anyhow::*;
use itertools::Itertools;
use log::*;
use owo_colors::OwoColorize;
use rayon::prelude::*;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

pub(crate) fn to_csv(cs: &ConstraintSet, exclude: &[String], filename: &str) -> Result<()> {
    let base_filename = Path::new(filename);

    cs.columns
        .modules()
        .par_iter()
        .map(|module| {
            if exclude.contains(&module) {
                return Ok(());
            }

            let filename = base_filename.with_file_name(format!(
                "{}_{}",
                base_filename.file_name().unwrap().to_str().unwrap(),
                module
            ));
            info!("Writing {}", filename.display());

            let mut file = BufWriter::new(File::create(&filename)?);

            info!("Exporting {}", module);
            let column_names = cs
                .columns
                .iter_module(&module)
                .map(|c| cs.handle(&c.0))
                .sorted()
                .collect::<Vec<_>>();

            file.write(column_names.iter().map(|h| &h.name).join(",").as_bytes())?;
            file.write(&[b'\n'])?;
            let max_i = cs.iter_len(&module);
            for i in 0..max_i {
                file.write(
                    cs.columns
                        .iter_module(&module)
                        .map(|col| {
                            cs.columns
                                .get(&col.0, i.try_into().unwrap(), false)
                                .unwrap_or_default()
                                .pretty_with_base(col.1.base)
                        })
                        .join(",")
                        .as_bytes(),
                )?;
                file.write(&[b'\n'])?;
            }

            Ok(file.flush()?)
        })
        .collect::<Result<_>>()
}

pub(crate) fn to_json(cs: &ConstraintSet, exclude: &[String], filename: &str) -> Result<()> {
    let mut out = BufWriter::new(
        File::create(filename).with_context(|| anyhow!("opening {}", filename.bold().yellow()))?,
    );
    out.write_all(b"{")?;
    let mut all_handles = cs
        .columns
        .iter()
        .filter(|cr| cr.1.kind == Kind::Commitment)
        .map(|cr| cr.1.handle.to_owned())
        .collect::<Vec<_>>();
    all_handles.sort_by(|a, b| a.module.cmp(&b.module));
    let modules = all_handles.iter().group_by(|h| &h.module);
    let mut modules = modules.into_iter().peekable();

    while let Some((module, handles)) = modules.next() {
        if exclude.contains(module) {
            continue;
        }

        out.write_all(format!("\"{}\": {{\"Trace\":{{", module).as_bytes())?;
        let mut handles = handles.into_iter().peekable();
        while let Some(handle) = handles.next() {
            out.write_all(format!("\"{}\": [\n", &handle.name).as_bytes())?;
            let empty_backing: ValueBacking = ValueBacking::default();
            let backing = cs
                .columns
                .backing(&ColumnRef::from_handle(handle.clone()))
                .unwrap_or(&empty_backing);
            let values = backing
                .iter_without_spilling(&cs.columns)
                .map(|x| format!("\"{}\"", x.pretty_with_base(Base::Dec)))
                .join(",");
            out.write_all(values.as_bytes())?;

            out.write_all(b"]")?;
            if handles.peek().is_some() {
                out.write_all(b",")?;
            }
            out.write_all(b"\n")?;
        }
        out.write_all(b"}}")?;
        if modules.peek().is_some() {
            out.write_all(b",")?;
        }
        out.write_all(b"\n")?;
    }
    out.write_all(b"}")?;
    Ok(())
}
