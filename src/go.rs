use crate::parser::*;
use color_eyre::eyre::*;

#[derive(Debug)]
pub(crate) struct GoExporter {
    pub settings: crate::Args,
}
impl GoExporter {
    fn make_chain(&self, xs: &[Constraint], operand: &str, surround: bool) -> Result<String> {
        let head = self.render_node(&xs[0])?;
        let tail = xs[1..]
            .iter()
            .map(|x| self.render_node(x).map(|s| format!("{}({})", operand, s)))
            .collect::<Result<Vec<_>>>()?
            .join(".");
        let chain = format!("{}.{}", head, tail);
        Ok(if surround {
            format!("({})", chain)
        } else {
            chain
        })
    }
    pub fn render_node(&self, node: &Constraint) -> Result<String> {
        match node {
            Constraint::Const(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x @ _ => Ok(format!("column.CONST_UINT64({})", x)),
            },
            Constraint::Column(name) => Ok(format!("CE[{}.Name()]", name)),
            Constraint::Funcall { func, args } => self.render_funcall(func, args),
        }
    }
    pub fn render_funcall(&self, func: &Builtin, args: &[Constraint]) -> Result<String> {
        match func {
            Builtin::Add => self.make_chain(args, "Add", true),
            Builtin::Mul => self.make_chain(args, "Mul", false),
            Builtin::Sub => self.make_chain(args, "Sub", true),
            Builtin::Equals => self.make_chain(args, "Equals", true),

            Builtin::Defcolumns | Builtin::Defalias | Builtin::Defun | Builtin::Defunalias => {
                panic!("Should never happen")
            }
        }
    }
}

impl crate::parser::Transpiler for GoExporter {
    fn render(&self, cs: &ConstraintsSet) -> Result<String> {
        let prelude = format!(
            "package {}
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
            .map(|c| self.render_node(c).map(|s| format!("  {},", s)))
            .collect::<Result<Vec<_>>>()?
            .join("\n");

        let r = format!(
            "{}\nfunc {}() []column.Expression {{\n  return[]column.Expression{{\n {} }}\n}}\n",
            prelude, &self.settings.name, body
        );
        Ok(r)
    }
}
