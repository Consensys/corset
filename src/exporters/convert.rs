use crate::{
    compiler::ConstraintSet,
    pretty::{Base, Pretty},
    utils::purify,
};
use anyhow::*;
use itertools::Itertools;
use rusqlite::Connection;

pub(crate) fn to_sqlite(cs: &ConstraintSet, exclude: &[String], filename: &str) -> Result<()> {
    let db = Connection::open(filename)?;

    for module in cs.columns.modules() {
        if exclude.contains(&module) {
            continue;
        }
        println!("Exporting {}", &module);
        let column_names = cs
            .columns
            .iter_module(&module)
            .map(|c| cs.handle(&c.0))
            .collect::<Vec<_>>();
        let sql_column_headers = column_names
            .iter()
            .map(|h| format!("{}_ TEXT NOT NULL", h.name))
            .join(", ");
        let sql_column_names = column_names
            .iter()
            .map(|h| format!("{}_ ", h.name))
            .join(", ");
        db.execute(&format!("DROP TABLE IF EXISTS {}_", purify(&module)), ())?;
        db.execute(
            &format!("CREATE TABLE {}_ ({})", purify(&module), sql_column_headers),
            (),
        )?;
        let max_i = cs.iter_len(&module);
        if max_i == 0 {
            continue;
        }

        for i in 0..max_i {
            let vals = cs
                .columns
                .iter_module(&module)
                .map(|col| {
                    format!(
                        "\"{}\"",
                        cs.columns
                            .get(&col.0, i.try_into().unwrap(), false)
                            .unwrap_or_default()
                            .pretty_with_base(Base::Hex)
                    )
                })
                .join(", ");
            db.execute(
                &format!(
                    "INSERT INTO {}_ ({}) VALUES ({})",
                    purify(&module),
                    &sql_column_names,
                    &vals
                ),
                (),
            )?;
        }
    }

    Ok(())
}
