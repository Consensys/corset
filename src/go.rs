use crate::parser::ConstraintsSet;
use color_eyre::eyre::*;

pub(crate) struct GoExporter {
    pub settings: crate::Args,
}

impl crate::parser::Exporter for GoExporter {
    fn render(&self, cs: &ConstraintsSet) -> Result<String> {
        let prelude = format!(
            " package {}
import (
    \"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column\"
    // \"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/constraint\"
    // \"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/module\"
)
",
            self.settings.package
        );

        let body = cs
            .constraints
            .iter()
            .map(|c| c.render().map(|s| format!("  {},", s)))
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let r = format!(
            "{}\nfunc {}() []column.Expression {{\n  return[]column.Expression{{\n {} }}\n}}\n",
            prelude, &self.settings.name, body
        );
        Ok(r)
    }
}
