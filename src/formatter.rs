use std::matches;
use std::unreachable;

use crate::compiler::codetyper::Tty;
use crate::compiler::tables::BUILTINS;
use crate::compiler::Ast;
use crate::compiler::AstNode;
use crate::compiler::Token;

/// Number of empty lines between top-level definitions
const TOPLEVEL_SPACING: usize = 1;

/// Try not to generate lines longer than this value
const MAX_LINE_WIDTH: usize = 110;

static mut NOFORMAT: bool = false;

impl Tty {
    fn each_but_last<
        X,
        I: Iterator<Item = X>,
        F1: FnMut(&X, &mut Self),
        F2: FnMut(&X, &mut Self),
    >(
        &mut self,
        iter: I,
        mut all: F1,
        mut but_last: F2,
    ) {
        let mut iter = iter.peekable();
        while let Some(x) = iter.next() {
            all(&x, self);
            if let Some(y) = iter.peek() {
                but_last(&y, self);
            }
        }
    }

    fn each_but_and_last<
        X: Copy,
        I: Iterator<Item = X>,
        F1: FnMut(X, &mut Self),
        F2: FnMut(X, &mut Self),
        F3: FnMut(X, &mut Self),
    >(
        &mut self,
        iter: I,
        mut all: F1,
        mut but_last: F2,
        mut and_last: F3,
    ) {
        let mut iter = iter.peekable();
        while let Some(x) = iter.next() {
            all(x, self);
            if let Some(next) = iter.peek() {
                but_last(*next, self);
            } else {
                and_last(x, self);
            }
        }
    }
}

fn format_ifeq(l: &AstNode, tty: &mut Tty) {
    let ns = l.as_list().unwrap();
    let fname = ns[0].as_symbol().unwrap();
    // put the two first args on the same line
    tty.within(fname, Some(fname.len() + 2), |tty| {
        ns[1].format(tty);
        tty.write(" ");
        tty.each_but_and_last(
            ns.iter().skip(2),
            |arg, tty| {
                arg.format(tty);
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    spacer(tty, !merge(l, tty.indentation(), false, false))
                }
            },
            |a, tty| {
                if a.is_block_comment() {
                    tty.cr();
                }
            },
        )
    });
}

fn format_user_funcall(content: &[AstNode], tty: &mut Tty) {
    struct ArgsCounter {
        max_length: usize,
        max_count: usize,
        length: usize,
        count: usize,
    }
    impl ArgsCounter {
        fn new(x: usize, max_l: usize, max_c: usize) -> Self {
            Self {
                max_length: max_l,
                max_count: max_c,
                length: x,
                count: 0,
            }
        }
        fn increment(&mut self, a: &AstNode) {
            self.length += a.len();
            self.count += 1;
        }
        fn reset(&mut self, x: usize) {
            self.length = x;
            self.count = 0;
        }
        fn overflow(&self) -> bool {
            self.length > self.max_length || self.count >= self.max_count
        }
    }

    let fname = content[0].as_symbol().unwrap();
    let args = &content[1..];
    tty.within(fname, Some(0), |tty| {
        let base_length = tty.indentation() + 3;
        let mut counter = ArgsCounter::new(base_length, MAX_LINE_WIDTH, 4);
        let mut args = args.iter().peekable();

        while let Some(a) = args.next() {
            if a.is_comment() {
                a.format(tty);
                tty.cr();
                counter.reset(base_length);
            } else {
                counter.increment(a);
                a.format(tty);
                if args.peek().is_some() {
                    if counter.overflow() {
                        tty.cr();
                        counter.reset(base_length);
                    } else {
                        tty.write(" ")
                    }
                }
            }
        }
    })
}

fn format_defpermutation(xs: &[AstNode], tty: &mut Tty) {
    tty.within("defpermutation", None, |tty| {
        tty.cr();
        tty.each_but_last(
            xs.iter().skip(1),
            |x, tty| {
                if x.as_list().is_ok() {
                    format_headless_list(x, tty);
                    tty.cr();
                } else {
                    x.format(tty);
                }
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    tty.cr()
                }
            },
        )
    });
}

fn format_definterleaved(xs: &[AstNode], tty: &mut Tty) {
    tty.within("definterleaved", None, |tty| {
        tty.cr();
        tty.each_but_last(
            xs.iter().skip(1),
            |x, tty| {
                x.format(tty);
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    tty.cr()
                }
            },
        )
    });
}

fn format_defcolumns(xs: &[AstNode], tty: &mut Tty) {
    tty.within("defcolumns", None, |tty| {
        tty.cr();
        tty.each_but_last(
            xs.iter().skip(1),
            |x, tty| {
                x.format(tty);
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    tty.cr()
                }
            },
        );
    });
}

fn format_defplookup(xs: &[AstNode], tty: &mut Tty) {
    tty.within("defplookup", None, |tty| {
        tty.cr();
        tty.each_but_last(
            xs.iter().skip(1),
            |x, tty| {
                if let Ok(columns) = x.as_list() {
                    within_early_unshift("", Some(2), tty, |tty| {
                        tty.cr();
                        let mut columns = columns.iter().peekable();
                        while let Some(column) = columns.next() {
                            column.format(tty);
                            if columns.peek().is_some() {
                                tty.cr();
                            }
                        }
                    });
                } else {
                    x.format(tty);
                }
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    tty.cr()
                }
            },
        )
    });
}

fn format_defperspective(xs: &[AstNode], tty: &mut Tty) {
    tty.within("defperspective", None, |tty| {
        tty.each_but_last(
            xs.iter().skip(1),
            |x, tty| match &x.class {
                Token::BlockComment(_) => {
                    tty.cr();
                    x.format(tty);
                }
                Token::List(_) => {
                    format_headless_list(x, tty);
                }
                _ => {
                    x.format(tty);
                }
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    tty.cr()
                }
            },
        );
    });
}

fn format_defun(xs: &[AstNode], tty: &mut Tty) {
    let fname = xs[0].as_symbol().unwrap();
    tty.within(fname, None, |tty| {
        if let Some(prototype) = xs.get(1) {
            tty.indented(fname.len(), |tty| {
                if let Token::List(_) = prototype.class {
                    format_list(prototype, false, false, true, tty);
                } else {
                    prototype.format(tty);
                }
            });
            tty.cr();
        };

        for rest in xs.iter().skip(2) {
            rest.format(tty);
            if matches!(rest.class, Token::BlockComment(_)) {
                tty.cr();
            }
        }
    });
}

fn format_defconstraint(xs: &[AstNode], tty: &mut Tty) {
    tty.within("defconstraint", None, |tty| {
        if let Some(name) = xs.get(1) {
            name.format(tty);
        };

        if let Some(settings) = xs.get(2) {
            tty.write(" ");
            if let Token::List(_) = settings.class {
                format_list(settings, false, true, false, tty);
            } else {
                settings.format(tty);
            }
        }

        tty.cr();
        for rest in xs.iter().skip(3) {
            rest.format(tty);
            if matches!(rest.class, Token::BlockComment(_)) {
                tty.cr();
            }
        }
    });
}

fn format_let(n: &[AstNode], tty: &mut Tty) {
    tty.within("let", Some("let".len() + 2), |tty| {
        if let Some(ls) = n.get(1).and_then(|ls| ls.as_list().ok()) {
            tty.within("", Some(1), |tty| {
                tty.each_but_last(
                    ls.iter(),
                    |l, tty| {
                        l.format(tty);
                    },
                    |x, tty| {
                        if !x.is_inline_comment() {
                            tty.cr()
                        }
                    },
                )
            });
            tty.cr();
            for n in n.iter().skip(2) {
                n.format(tty);
                if matches!(n.class, Token::BlockComment(_)) {
                    tty.cr();
                }
            }
        } else {
            for n in n.iter().skip(1) {
                n.format(tty);
                if matches!(n.class, Token::BlockComment(_)) {
                    tty.cr();
                }
            }
        }
    });
}

fn merge(n: &AstNode, current_indent: usize, force_newline: bool, force_no_newline: bool) -> bool {
    (n.depth() <= 2 && n.len() + current_indent <= MAX_LINE_WIDTH)
        && !n
            .as_list()
            .map(|l| l.iter().any(|n| n.is_comment()))
            .unwrap_or(true)
        && !force_newline
        || force_no_newline
}

fn format_headless_list(n: &AstNode, tty: &mut Tty) {
    let ns = n.as_list().unwrap();
    tty.within("", Some(1), |tty| {
        tty.each_but_and_last(
            ns.iter(),
            |a, tty| {
                a.format(tty);
            },
            |x, tty| {
                if !x.is_inline_comment() {
                    spacer(tty, true)
                }
            },
            |a, tty| {
                if a.is_block_comment() {
                    tty.cr();
                }
            },
        )
    })
}

fn format_defpairs(xs: &[AstNode], tty: &mut Tty) {
    tty.within(xs[0].as_symbol().unwrap(), None, |tty| {
        let mut i = true;
        let mut max_length = 0;
        tty.cr();
        for n in xs[1..].iter().filter(|n| !n.is_comment()) {
            if i {
                if let Ok(name) = n.as_symbol() {
                    max_length = max_length.max(name.len());
                }
            }
            i = !i;
        }
        // add a space between constant name & value
        max_length += 1;

        // let max_length = xs[1..].iter().filter(predicate);
        i = true;
        let mut iter = xs[1..].iter().peekable();
        while let Some(n) = iter.next() {
            if n.is_comment() {
                n.format(tty);
                tty.cr();
            } else {
                if i {
                    if let Ok(name) = n.as_symbol() {
                        tty.write(&format!("{:1$}", name, max_length));
                    } else {
                        n.format(tty);
                    }
                } else {
                    n.format(tty);
                }

                match iter.peek() {
                    Some(a) => match &a.class {
                        Token::InlineComment(_) => {
                            a.format(tty);
                            iter.next();
                        }
                        _ => {}
                    },
                    None => {}
                };

                if matches!(n.class, Token::BlockComment(_)) || (!i && iter.peek().is_some()) {
                    tty.cr();
                }
                i = !i;
            }
        }
    });
}

fn format_list(
    n: &AstNode,
    force_newline: bool,
    force_no_newline: bool,
    maybe_funcall: bool,
    tty: &mut Tty,
) {
    const UNMERGING_FNAMES: &[&str] = &["if-zero", "if", "if-not-zero", "begin"];
    let reserved_names = BUILTINS
        .keys()
        .chain(["if-zero", "if-not-zero", "if", "if-eq", "vanishes!"].iter())
        .cloned()
        .collect::<Vec<_>>();

    let ns = n.as_list().unwrap();

    if ns.len() <= 1 {
        tty.within("", None, |tty| {
            for n in ns.iter() {
                n.format(tty);
            }
        })
    } else {
        let merge = merge(n, tty.indentation(), force_newline, force_no_newline);
        if let Ok(fname) = ns[0].as_symbol() {
            if false
            // maybe_funcall
            // && ns.len() > 1
            // && !reserved_names.contains(&fname)
            // && !force_no_newline
            {
                format_user_funcall(ns, tty);
            } else {
                let merge = if UNMERGING_FNAMES.contains(&fname) {
                    false
                } else {
                    merge
                };
                tty.within(fname, Some(fname.len() + 2), |tty| {
                    tty.each_but_and_last(
                        ns.iter().skip(1),
                        |arg, tty| {
                            arg.format(tty);
                        },
                        |x, tty| {
                            if !x.is_inline_comment() && ns.len() > 2 {
                                spacer(tty, !merge)
                            }
                        },
                        |a, tty| {
                            if a.is_block_comment() {
                                tty.cr();
                            }
                        },
                    );
                });
            }
        } else {
            tty.within("", Some(1), |tty| {
                tty.each_but_and_last(
                    ns.iter(),
                    |a, tty| {
                        a.format(tty);
                    },
                    |x, tty| {
                        if !x.is_inline_comment() {
                            spacer(tty, !merge)
                        }
                    },
                    |a, tty| {
                        if a.is_block_comment() {
                            tty.cr();
                        }
                    },
                )
            })
        }
    }
}

fn within_early_unshift<F: Fn(&mut Tty)>(
    label: &str,
    indent: Option<usize>,
    tty: &mut Tty,
    what: F,
) {
    tty.write(format!(
        "({}{}",
        label,
        if label.is_empty() { "" } else { " " }
    ));
    tty.shift(indent.unwrap_or(2));

    what(tty);

    tty.unshift();
    tty.cr();
    tty.write(")");
}

fn spacer(tty: &mut Tty, with_newlines: bool) {
    if with_newlines {
        tty.cr();
    } else {
        tty.write(" ");
    }
}

impl Ast {
    pub fn format(&self) -> String {
        let mut tty = Tty::new().align_annotations().default_indent(2);
        let mut exprs = self.exprs.iter().peekable();
        while let Some(n) = exprs.next() {
            n.format(&mut tty);
            while exprs.peek().map(|n| n.is_inline_comment()).unwrap_or(false) {
                exprs.next().unwrap().format(&mut tty);
            }

            tty.cr();
            if exprs.peek().is_some() && !n.is_comment() {
                for _ in 0..TOPLEVEL_SPACING {
                    tty.cr();
                }
            }
        }
        tty.cr();

        tty.page_feed()
    }
}
impl AstNode {
    fn len(&self) -> usize {
        match &self.class {
            Token::BlockComment(_) => 0,
            Token::Value(x) => x.to_string().len(),
            Token::Symbol(s) | Token::Keyword(s) => s.len(),
            Token::List(ns) => ns.iter().map(|n| n.len() + 1).sum::<usize>() + 2,
            Token::Range(domain) => {
                domain
                    .iter()
                    .map(|d| d.to_string().len() + 1)
                    .sum::<usize>()
                    + 1
            }
            Token::DefModule(m) => 2 + "module".len() + 1 + m.len(),
            _ => 0,
        }
    }

    pub fn format(&self, tty: &mut Tty) {
        unsafe {
            NOFORMAT = if NOFORMAT {
                tty.each_but_last(self.src.lines(), |l, tty| tty.write(l), |_, tty| tty.cr());
                false
            } else {
                match &self.class {
                    Token::BlockComment(c) => {
                        let mut lines = c.lines().peekable();
                        let pragma = c.lines().last().unwrap().trim_start_matches(';').trim();
                        while let Some(c) = lines.next() {
                            tty.write(c);
                            if lines.peek().is_some() {
                                tty.cr();
                            }
                        }

                        match pragma {
                            "corset:noformat" => true,
                            _ => false,
                        }
                    }
                    Token::InlineComment(c) => {
                        tty.annotate(c.to_owned());
                        false
                    }
                    Token::Value(_) => {
                        tty.write(&self.src);
                        false
                    }
                    Token::Symbol(s) => {
                        tty.write(s);
                        false
                    }
                    Token::Keyword(kw) => {
                        tty.write(kw.to_lowercase());
                        false
                    }
                    Token::IndexedSymbol { name, index } => {
                        tty.write(format!("[{} {}]", name, index));
                        false
                    }
                    Token::List(ns) => {
                        match ns.get(0).and_then(|x| x.as_symbol().ok()) {
                            Some("defun") | Some("defpurefun") => format_defun(ns, tty),
                            Some("defconstraint") => format_defconstraint(ns, tty),
                            Some("defcolumns") => format_defcolumns(ns, tty),
                            Some("defconst") | Some("defalias") | Some("defunalias") => {
                                format_defpairs(ns, tty)
                            }
                            Some("defperspective") => format_defperspective(ns, tty),
                            Some("defplookup") => format_defplookup(ns, tty),
                            Some("defpermutation") => format_defpermutation(ns, tty),
                            Some("definterleaved") => format_definterleaved(ns, tty),
                            Some("let") => format_let(ns, tty),
                            Some("module") | Some("definrange") => {
                                format_list(self, false, true, false, tty);
                            }
                            Some("if-eq") | Some("if-eq-else") => format_ifeq(self, tty),
                            _ => format_list(self, false, false, true, tty),
                        };
                        false
                    }
                    Token::Range(_) => {
                        tty.write(&self.src);
                        false
                    }
                    x => unreachable!("{:?}", &x),
                }
            }
        }
    }
}
