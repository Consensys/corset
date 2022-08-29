use crate::parser::*;
use color_eyre::eyre::*;

#[derive(Debug)]
pub(crate) struct GoExporter {
    pub settings: crate::Args,
}
impl GoExporter {
    fn make_chain(&self, xs: &[AstNode], operand: &str) -> Result<String> {
        let head = self.render_node(&xs[0])?;
        let tail = xs[1..]
            .iter()
            .map(|x| self.render_node(x).map(|s| format!("{}({})", operand, s)))
            .collect::<Result<Vec<_>>>()?
            .join(".");
        Ok(format!("{}{}", head, tail))
    }
    pub fn render_node(&self, node: &AstNode) -> Result<String> {
        match node {
            AstNode::Value(x) => match x {
                0..=2 | 127 | 256 => Ok(format!("column.CONST_{}()", x)),
                x @ _ => Ok(format!("column.CONST_UINT64({})", x)),
            },
            AstNode::Column(name) => Ok(format!("CE[{}.Name()]", name)),
            AstNode::Funcall { verb, args } => self.render_funcall(verb, args),
            _ => Ok("??".into()),
        }
    }
    pub fn render_funcall(&self, func: &Verb, args: &[AstNode]) -> Result<String> {
        match func {
            Verb::Ready(builtin) => match builtin {
                Builtin::BranchIfZero => {
                    let cond = self.render_node(&args[0])?;
                    let then = args[1..]
                        .iter()
                        .map(|n| self.render_node(n))
                        .collect::<Result<Vec<_>>>()?
                        .join(", ");
                    Ok(format!("{}.BranchIfZero({})", cond, then))
                }
                Builtin::Vanishes => {
                    assert!(args.len() == 1);
                    self.render_node(&args[0])
                }
                Builtin::Add => self.make_chain(args, ".Add"),
                Builtin::And => self.make_chain(args, ".Mul"),
                Builtin::Mul => self.make_chain(args, ".Mul"),
                Builtin::Sub => self.make_chain(args, ".Sub"),
                Builtin::Equals => self.make_chain(args, ".Equals"),
                Builtin::BinIfOne => {
                    if args.len() != 2 {
                        Err(anyhow!("if-one takes two argument {}", 3))
                    } else {
                        let lhs = self.render_node(&args[0])?;
                        let rhs = self.render_node(&args[1])?;
                        Ok(format!("{}.BinIfOne({})", lhs, rhs))
                    }
                }
                Builtin::BinIfZero => {
                    if args.len() != 2 {
                        Err(anyhow!("if-one takes two argument {}", 3))
                    } else {
                        let lhs = self.render_node(&args[0])?;
                        let rhs = self.render_node(&args[1])?;
                        Ok(format!("{}.BinIfZero({})", lhs, rhs))
                    }
                }
                x @ _ => {
                    dbg!(x);
                    todo!()
                }
            },
            Verb::Raw(name) => Ok(format!("Unknown raw function: {}", name)),
        }
    }
}

impl crate::parser::Transpiler for GoExporter {
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
