use std::format;
use std::todo;

use itertools::Itertools;

use crate::compiler::codetyper::Tty;
use crate::compiler::Ast;
use crate::compiler::AstNode;
use crate::compiler::Magma;
use crate::compiler::Token;
use crate::compiler::Type;
use crate::pretty::Base;

fn magma_to_kw(m: Magma) -> String {
    match m {
        Magma::None => "",
        Magma::Boolean => ":boolean",
        Magma::Loobean => ":loobean",
        Magma::Nibble => ":nibble",
        Magma::Byte => ":byte",
        Magma::Integer => ":integer",
        Magma::Any => unreachable!(),
    }
    .into()
}

fn base_to_kw(b: Base) -> String {
    match b {
        Base::Dec => ":base :dec",
        Base::Hex => ":base :hex",
        Base::Bin => ":base :bin",
        Base::Bytes => ":base :bytes",
    }
    .into()
}

fn format_defunction(
    def: &str,
    name: &str,
    args: &[String],
    in_types: &[Type],
    out_type: &Option<Type>,
    body: &AstNode,
    nowarn: bool,
    tty: &mut Tty,
) {
    let fmt_name = if nowarn || out_type.is_some() {
        [
            Some(name.clone()),
            out_type.map(|t| magma_to_kw(t.magma())).as_deref(),
            if nowarn { Some(":nowarn") } else { None },
        ]
        .into_iter()
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .join(" ")
    } else {
        name.to_string()
    };

    let fmt_args = args
        .iter()
        .enumerate()
        .map(|(i, name)| {
            if matches!(in_types[i], Type::Any(Magma::Any)) {
                name.clone()
            } else {
                format!("({} {})", name, magma_to_kw(in_types[i].magma()))
            }
        })
        .join(" ");

    tty.write(format!("(defpurefun ({fmt_name} {fmt_args})"));
}

impl Ast {
    pub fn format(&self) -> String {
        self.exprs.iter().map(AstNode::format).join("")
    }
}
impl AstNode {
    fn len(&self) -> usize {
        match &self.class {
            Token::Comment(_) => 0,
            Token::Value(x) => x.to_string().len(),
            Token::Symbol(s) | Token::Keyword(s) => s.len(),
            Token::List(ns) => ns.iter().map(|n| n.len() + 1).sum::<usize>() + 1,
            Token::Range(domain) => {
                domain
                    .iter()
                    .map(|d| d.to_string().len() + 1)
                    .sum::<usize>()
                    + 1
            }
            Token::DefModule(m) => 2 + 6 + 1 + m.len(),
            Token::DefConsts(_) => todo!(),
            Token::DefColumns(_) => todo!(),
            Token::DefPerspective {
                name,
                trigger,
                columns,
            } => todo!(),
            Token::DefColumn {
                name,
                t,
                kind,
                padding_value,
                base,
            } => todo!(),
            Token::DefArrayColumn {
                name,
                domain,
                t,
                base,
            } => todo!(),
            Token::Defun {
                name,
                args,
                in_types,
                out_type,
                body,
                nowarn,
            } => todo!(),
            Token::Defpurefun {
                name,
                args,
                in_types,
                out_type,
                body,
                nowarn,
            } => todo!(),
            Token::DefAliases(_) => todo!(),
            Token::DefAlias(_, _) => todo!(),
            Token::DefunAlias(_, _) => todo!(),
            Token::DefConstraint {
                name,
                domain,
                guard,
                perspective,
                body,
            } => todo!(),
            Token::DefPermutation { from, to, signs } => todo!(),
            Token::DefPlookup {
                name,
                including,
                included,
            } => todo!(),
            Token::DefInrange(_, _) => todo!(),
        }
    }
    pub fn format(&self) -> String {
        fn spacer(tty: &mut Tty, with_newlines: bool) {
            if with_newlines {
                tty.cr();
            } else {
                tty.write(" ");
            }
        }

        fn _format(n: &AstNode, tty: &mut Tty) {
            match &n.class {
                Token::Comment(c) => {
                    tty.write(format!("{c}"));
                    tty.cr();
                }
                Token::Value(_) => tty.write(&n.src),
                Token::Symbol(s) => tty.write(s),
                Token::Keyword(kw) => tty.write(kw),
                Token::List(ns) => {
                    if ns.len() <= 1 {
                        tty.write("(");
                        for n in ns.iter() {
                            _format(n, tty);
                        }
                        tty.write(")");
                    } else {
                        let merge = (n.depth() < 2 && n.len() + tty.indentation() <= 10000)
                            || (n.len() < 50);
                        if let Some(fname) = ns[0].as_symbol().ok() {
                            tty.write(format!("({fname} "));
                            tty.shift(fname.len() + 2);
                            let mut args = ns.iter().skip(1).peekable();
                            while let Some(a) = args.next() {
                                _format(a, tty);
                                if args.peek().is_some() {
                                    spacer(tty, !merge);
                                }
                            }
                            tty.unshift();
                            tty.write(")");
                        } else {
                            tty.write(format!("("));
                            tty.shift(1);
                            let mut args = ns.iter().peekable();
                            while let Some(a) = args.next() {
                                _format(a, tty);
                                if args.peek().is_some() {
                                    spacer(tty, !merge);
                                }
                            }
                            tty.unshift();
                            tty.write(")");
                        }
                    }
                }
                Token::Range(_) => tty.write(&n.src),
                Token::DefModule(m) => {
                    tty.write(&format!("(module {m})"));
                    tty.cr()
                }
                Token::DefConsts(constants) => {
                    tty.write("(defconst");
                    if constants.len() > 1 {
                        tty.shift(2);
                        tty.cr();
                    } else {
                        tty.write(" ");
                    }

                    let (name, value): (Vec<&String>, Vec<String>) = constants
                        .iter()
                        .map(|(name, value)| (name, value.to_string()))
                        .unzip();
                    let largest = name.iter().map(|s| s.len()).max().unwrap_or_default();
                    let mut constants = name.iter().zip(value.iter()).peekable();
                    while let Some((name, value)) = constants.next() {
                        tty.write(&format!("{:2$} {}", name, value, largest));
                        if constants.peek().is_some() {
                            tty.cr();
                        }
                    }

                    tty.write(")");
                    if constants.len() > 1 {
                        tty.unshift();
                    }

                    tty.cr();
                }
                Token::DefColumns(cols) => {
                    tty.write("(defcolumns");
                    tty.shift(2);
                    tty.cr();
                    let mut defcols = cols.iter().peekable();
                    while let Some(defcol) = defcols.next() {
                        _format(defcol, tty);
                        if defcols.peek().is_some() {
                            tty.cr();
                        }
                    }
                    tty.write(")");
                    tty.unshift();
                    tty.cr();
                }
                Token::DefPerspective {
                    name,
                    trigger,
                    columns,
                } => todo!(),
                Token::DefColumn {
                    name,
                    t,
                    kind,
                    padding_value,
                    base,
                } => {
                    let with_opts = !matches!(t, Type::Column(Magma::Integer))
                        || padding_value.is_some()
                        || !matches!(base, Base::Dec);
                    if with_opts {
                        tty.write("(");
                    }
                    tty.write(name);
                    if !matches!(t, Type::Column(Magma::Integer)) {
                        tty.write(&format!(" {}", magma_to_kw(t.magma())));
                    }
                    if !matches!(base, Base::Dec) {
                        tty.write(&format!(" {}", base_to_kw(*base)));
                    }
                    if let Some(x) = padding_value {
                        tty.write(&format!(" :padding {}", x));
                    }
                    if with_opts {
                        tty.write(")");
                    }
                }
                Token::DefArrayColumn {
                    name,
                    domain,
                    t,
                    base,
                } => todo!(),
                Token::Defun {
                    name,
                    args,
                    in_types,
                    out_type,
                    body,
                    nowarn,
                } => {
                    format_defunction("defun", name, args, in_types, out_type, body, *nowarn, tty);
                    _format(body, tty);
                    tty.write(")");
                }
                Token::Defpurefun {
                    name,
                    args,
                    in_types,
                    out_type,
                    body,
                    nowarn,
                } => {
                    format_defunction(
                        "defpurefun",
                        name,
                        args,
                        in_types,
                        out_type,
                        body,
                        *nowarn,
                        tty,
                    );
                    if body.depth() > 1 {
                        tty.shift(2);
                        tty.cr();
                    }
                    _format(body, tty);
                    if body.depth() > 1 {
                        tty.unshift();
                    }
                    tty.write(")");
                    tty.cr();
                }
                Token::DefAliases(aliases) => {
                    tty.write("(defalias");
                    if aliases.len() > 1 {
                        tty.shift(2);
                        tty.cr();
                    } else {
                        tty.write(" ");
                    }

                    let (sources, targets): (Vec<&String>, Vec<&String>) = aliases
                        .iter()
                        .map(|defalias| {
                            let Token::DefAlias(source, target) = &defalias.class else {
                            unreachable!()
                        };
                            (source, target)
                        })
                        .unzip();
                    let largest = sources.iter().map(|s| s.len()).max().unwrap_or_default();
                    let mut aliases = sources.iter().zip(targets.iter()).peekable();
                    while let Some((source, target)) = aliases.next() {
                        tty.write(&format!("{:2$} {}", source, target, largest));
                        if aliases.peek().is_some() {
                            tty.cr();
                        }
                    }

                    tty.write(")");
                    if aliases.len() > 1 {
                        tty.unshift();
                    }

                    tty.cr();
                    tty.cr();
                }
                Token::DefunAlias(source, target) => {
                    tty.write(&format!("(defunalias {} {})", source, target));
                    tty.cr();
                }
                Token::DefConstraint {
                    name,
                    domain,
                    guard,
                    perspective,
                    body,
                } => {
                    let opts = [
                        domain.as_ref().map(|domain| {
                            format!(
                                ":domain {{{}}}",
                                domain.iter().map(|x| x.to_string()).join(" ")
                            )
                        }),
                        perspective
                            .as_ref()
                            .map(|perspective| format!(":perspective {perspective}")),
                        {
                            guard.as_ref().map(|guard| {
                                let mut opts_tty = Tty::new();
                                opts_tty.write(":guard ");
                                _format(guard, &mut opts_tty);
                                opts_tty.page_feed()
                            })
                        },
                    ]
                    .into_iter()
                    .filter(|x| x.is_some())
                    .map(|x| x.unwrap())
                    .join(" ");

                    tty.write(&format!("(defconstraint {name} ({opts})"));
                    if body.depth() > 0 {
                        tty.shift(2);
                        tty.cr();
                    }

                    _format(body, tty);
                    tty.write(&format!(")"));

                    if body.depth() > 0 {
                        tty.cr();
                        tty.unshift();
                    }
                    tty.cr();
                }
                Token::DefPermutation { from, to, signs } => todo!(),
                Token::DefPlookup {
                    name,
                    including,
                    included,
                } => todo!(),
                Token::DefInrange(_, _) => todo!(),

                Token::DefAlias(..) => unreachable!(),
            }
        }

        let mut tty = Tty::new();
        _format(self, &mut tty);
        tty.page_feed()
    }
}
