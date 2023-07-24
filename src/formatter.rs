use crate::compiler::codetyper::Tty;
use crate::compiler::Ast;
use crate::compiler::AstNode;
use crate::compiler::Magma;
use crate::compiler::Token;
use crate::compiler::Type;
use crate::pretty::Base;
use itertools::Itertools;

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
        Base::Dec => ":display :dec",
        Base::Hex => ":display :hex",
        Base::Bin => ":display :bin",
        Base::Bytes => ":display :bytes",
    }
    .into()
}

fn format_defunction(
    def: &str,
    name: &str,
    args: &[String],
    in_types: &[Type],
    out_type: &Option<Type>,
    nowarn: bool,
    tty: &mut Tty,
) {
    let fmt_name = if nowarn || out_type.is_some() {
        [
            Some(name),
            out_type.map(|t| magma_to_kw(t.magma())).as_deref(),
            if nowarn { Some(":nowarn") } else { None },
        ]
        .into_iter()
        .flatten()
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

    tty.write(format!(
        "({def} ({fmt_name}{}{fmt_args})",
        if fmt_args.is_empty() { "" } else { " " }
    ));
}

impl Ast {
    pub fn format(&self) -> String {
        self.exprs.iter().map(AstNode::format).join("\n")
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
            _ => 0,
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

        fn maybe_comment(n: &AstNode, tty: &mut Tty) {
            if let Some(comment) = n.annotation.as_ref() {
                tty.buffer_end(comment.to_owned());
            }
        }

        fn _format(n: &AstNode, tty: &mut Tty) {
            match &n.class {
                Token::Comment(c) => {
                    let mut lines = c.lines().peekable();
                    while let Some(c) = lines.next() {
                        tty.write(c);
                        if lines.peek().is_some() {
                            tty.cr();
                        }
                    }
                }
                Token::Value(_) => {
                    tty.write(&n.src);
                    maybe_comment(n, tty);
                }
                Token::Symbol(s) => {
                    tty.write(s);
                    maybe_comment(n, tty);
                }
                Token::Keyword(kw) => {
                    tty.write(kw);
                    maybe_comment(n, tty);
                }
                Token::List(ns) => {
                    if ns.len() <= 1 {
                        tty.write("(");
                        for n in ns.iter() {
                            _format(n, tty);
                        }
                        tty.write(")");
                    } else {
                        let merge = ((n.depth() < 2 && n.len() + tty.indentation() <= 10000)
                            || (n.depth() < 3 && n.len() < 50))
                            && !ns.iter().any(|n| n.annotation.is_some());
                        if let Ok(fname) = ns[0].as_symbol() {
                            tty.write(format!("({fname} "));
                            maybe_comment(n, tty);
                            if n.annotation.is_some() {
                                tty.shift(2);
                                tty.cr();
                            } else {
                                tty.shift(fname.len() + 2);
                            }
                            // TODO: burn it with fire
                            if fname.starts_with("if-eq") {
                                _format(&ns[1], tty);
                                tty.write(" ");
                                let mut args = ns.iter_with_comments().skip(2).peekable();
                                while let Some(a) = args.next() {
                                    _format(a, tty);
                                    if args.peek().is_some() {
                                        spacer(tty, !merge);
                                    }
                                }
                            } else {
                                let mut args = ns.iter_with_comments().skip(1).peekable();
                                while let Some(a) = args.next() {
                                    _format(a, tty);
                                    if args.peek().is_some() {
                                        spacer(tty, !merge);
                                    }
                                }
                            }
                            tty.unshift();
                            tty.write(")");
                        } else {
                            tty.write("(");
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

                    let largest = constants
                        .iter()
                        .filter_map(|c| {
                            if let Token::DefConst(name, _) = &c.class {
                                Some(name.as_symbol().unwrap().len())
                            } else {
                                None
                            }
                        })
                        .max()
                        .unwrap_or_default();
                    let mut content = constants.iter().peekable();
                    while let Some(token) = content.next() {
                        match &token.class {
                            Token::DefConst(name, value) => {
                                tty.write(&format!("{:1$}", name, largest));
                                maybe_comment(token, tty);
                                spacer(tty, name.annotation.is_some());
                                _format(&value, tty);
                            }
                            Token::Comment(_) => _format(token, tty),
                            _ => unreachable!(),
                        };

                        if content.peek().is_some() {
                            tty.cr();
                        }
                    }

                    tty.write(")");
                    if constants.len() > 1 {
                        tty.unshift();
                    }

                    tty.cr();
                }
                Token::DefConst(..) => unreachable!(),
                Token::DefColumns(cols) => {
                    tty.write("(defcolumns");
                    tty.shift(2);
                    tty.cr();
                    let mut defcols = cols.iter_with_comments().peekable();
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
                Token::DefColumn {
                    name,
                    t,
                    padding_value,
                    base,
                    ..
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
                } => {
                    tty.write("(");
                    tty.write(name);
                    if !matches!(t, Type::ArrayColumn(Magma::Integer)) {
                        tty.write(&format!(" {}", magma_to_kw(t.magma())));
                    }
                    tty.write(&match domain {
                        crate::compiler::Domain::Range(start, stop) => {
                            if *start == 1 {
                                format!("[{}]", stop)
                            } else {
                                format!("[{}:{}]", start, stop)
                            }
                        }
                        crate::compiler::Domain::SteppedRange(start, step, stop) => {
                            format!("[{}:{}:{}]", start, stop, step)
                        }
                        crate::compiler::Domain::Set(_) => unreachable!(),
                    });
                    if !matches!(base, Base::Dec) {
                        tty.write(&format!(" {}", base_to_kw(*base)));
                    }
                    tty.write(")");
                }
                Token::DefPerspective {
                    name,
                    trigger,
                    columns,
                } => {
                    tty.write(format!("(defperspective {name}"));
                    tty.shift(2);
                    tty.cr();
                    _format(trigger, tty);
                    tty.cr();

                    tty.write("(");
                    tty.shift(1);
                    let mut defcols = columns.iter_with_comments().peekable();
                    while let Some(defcol) = defcols.next() {
                        _format(defcol, tty);
                        if defcols.peek().is_some() {
                            tty.cr();
                        }
                    }
                    tty.write(")");
                    tty.write(")");
                    tty.unshift();
                    tty.cr();
                }
                Token::Defun {
                    name,
                    args,
                    in_types,
                    out_type,
                    body,
                    nowarn,
                } => {
                    format_defunction("defun", name, args, in_types, out_type, *nowarn, tty);
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
                Token::Defpurefun {
                    name,
                    args,
                    in_types,
                    out_type,
                    body,
                    nowarn,
                } => {
                    format_defunction("defpurefun", name, args, in_types, out_type, *nowarn, tty);
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

                    let largest = aliases
                        .iter()
                        .map(|defalias| {
                            let Token::DefAlias(source, _) = &defalias.class else {
                            unreachable!()
                        };
                            source.len()
                        })
                        .max()
                        .unwrap_or(0);
                    let mut aliases_i = aliases.iter_with_comments().peekable();
                    while let Some(current) = aliases_i.next() {
                        if let Token::DefAlias(source, target) = &current.class {
                            tty.write(&format!("{:2$} {}", source, target, largest));
                        } else {
                            _format(current, tty);
                        }
                        if aliases_i.peek().is_some() {
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
                    .filter_map(|x| x.map(|x| x.split_whitespace().join(" ")))
                    .join(" ");

                    tty.write(&format!("(defconstraint {name} ({opts})"));

                    if body.depth() > 0 {
                        tty.shift(2);
                        tty.cr();
                    }

                    _format(body, tty);

                    tty.write(")");

                    if let Some(comment) = n.annotation.as_ref() {
                        tty.buffer_end(comment.to_owned());
                    }

                    if body.depth() > 0 {
                        tty.cr();
                        tty.unshift();
                    }
                    tty.cr();
                }
                Token::DefPermutation { from, to, signs } => {
                    tty.write("(defpermutation");
                    tty.shift(2);
                    tty.cr();

                    tty.write(&format!("({})", to.join(" ")));
                    tty.cr();

                    tty.write(&format!(
                        "({})",
                        from.iter()
                            .zip(signs.iter())
                            .map(|(col, sign)| if let Some(sign) = sign {
                                format!("({} {})", if *sign { "↓" } else { "↑" }, col)
                            } else {
                                col.to_string()
                            })
                            .join(" ")
                    ));

                    tty.write(")");
                    tty.unshift();
                    tty.cr();
                }
                Token::DefPlookup {
                    name,
                    including,
                    included,
                } => {
                    tty.write(&format!("(defplookup {name}"));
                    tty.shift(2);
                    tty.cr();

                    tty.write("(");
                    tty.shift(2);
                    for i in including.iter_with_comments() {
                        _format(i, tty);
                        tty.cr();
                    }
                    tty.unshift();
                    tty.cr();
                    tty.write(")");

                    tty.write("(");
                    tty.shift(2);
                    for i in included.iter_with_comments() {
                        _format(i, tty);
                        tty.cr();
                    }

                    tty.unshift();
                    tty.write(")");
                    tty.write(")");
                }
                Token::DefInrange(what, limit) => {
                    tty.write("(definrange ");
                    _format(what, tty);
                    tty.write(&format!(" {})", limit));
                }

                Token::DefAlias(..) => unreachable!(),
            }
        }

        let mut tty = Tty::new().align_annotations();
        _format(self, &mut tty);
        tty.page_feed()
    }
}
