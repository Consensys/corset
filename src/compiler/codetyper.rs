use itertools::Itertools;
use owo_colors::{colored::Color, OwoColorize};

#[derive(Debug)]
struct Line {
    indentation: usize,
    text: String,
    annotation: Option<String>,
}
impl Line {
    fn with_indent(indent: usize) -> Line {
        Line {
            indentation: indent,
            text: String::new(),
            annotation: None,
        }
    }
}
impl std::default::Default for Line {
    fn default() -> Self {
        Line {
            indentation: 0,
            text: String::new(),
            annotation: None,
        }
    }
}

pub struct Tty {
    default_indent: usize,
    with_guides: bool,
    align_annotations: bool,

    depths: Vec<Vec<usize>>,
    lines: Vec<Line>,
}

impl Tty {
    pub fn new() -> Self {
        Self {
            default_indent: 2,
            with_guides: false,
            align_annotations: false,
            depths: vec![vec![]],
            lines: vec![Default::default()],
        }
    }

    pub fn align_annotations(mut self) -> Self {
        self.align_annotations = true;
        self
    }

    pub fn with_guides(mut self) -> Self {
        self.with_guides = true;
        self
    }

    pub fn default_indent(mut self, x: usize) -> Self {
        self.default_indent = x;
        self
    }

    pub fn write<S: AsRef<str>>(&mut self, l: S) {
        let l = l.as_ref();
        if l.contains('\n') {
            panic!("newlines found in `{}`", l);
        }
        self.lines.last_mut().unwrap().text.push_str(l);
    }

    pub fn within<F: Fn(&mut Tty)>(&mut self, label: &str, indent: Option<usize>, what: F) {
        self.write(format!(
            "({}{}",
            label,
            if label.is_empty() { "" } else { " " }
        ));
        let indent = indent.unwrap_or(self.default_indent);
        self.shift(indent);

        what(self);

        self.write(")");
        self.unshift();
    }

    pub fn indented<F: Fn(&mut Tty)>(&mut self, indent: usize, what: F) {
        self.shift(indent);
        what(self);
        self.unshift();
    }

    pub fn shift(&mut self, d: usize) {
        let mut new_indent = self.depths.last().unwrap().clone();
        new_indent.push(d);
        self.depths.push(new_indent);
    }

    pub fn indentation(&self) -> usize {
        self.depths.last().unwrap().iter().sum()
    }

    fn indentation_for(&self, l: &Line) -> usize {
        self.depths.get(l.indentation).unwrap().iter().sum()
    }

    pub fn unshift(&mut self) {
        let mut new_indent = self.depths.last().unwrap().clone();
        new_indent.pop();
        self.depths.push(new_indent);
    }

    pub fn annotate(&mut self, s: String) {
        for putative in self.lines.iter_mut().rev() {
            if !putative.text.is_empty() {
                if let Some(annotation) = putative.annotation.as_mut() {
                    annotation.push_str(&s)
                } else {
                    putative.annotation = Some(s.clone())
                }
                return;
            }
        }
        panic!()
    }

    pub fn cr(&mut self) {
        self.lines.push(Line::with_indent(self.depths.len() - 1));
    }

    fn make_indent(&self, l: &Line) -> String {
        if self.with_guides {
            " ".to_string() // Account for the skipped first '|'
            + &self
                .depths[l.indentation]
                .iter()
                .skip(1)
                .map(|d| " ".repeat((*d as isize - 1).max(0) as usize))
                .collect::<Vec<_>>()
                .join("│".color(Color::BrightBlack).to_string().as_str())
        } else {
            " ".repeat(self.indentation_for(&l))
        }
    }

    pub fn page_feed(&mut self) -> String {
        let r = if !self.align_annotations {
            self.lines
                .iter()
                .map(|line| {
                    format!(
                        "{}{}",
                        line.text,
                        line.annotation
                            .as_ref()
                            .map(|s| s.as_str())
                            .unwrap_or_default()
                    )
                })
                .join("\n")
        } else {
            let mut max_len = 0;
            self.lines
                .iter()
                .enumerate()
                .map(|(i, l)| {
                    if self.indentation_for(l) == 0 {
                        max_len = 0;
                        for j in i + 1..self.lines.len() {
                            if self.indentation_for(&self.lines[j]) == 0 {
                                break;
                            } else {
                                if self.lines[j].annotation.is_some() {
                                    max_len = max_len.max(
                                        self.indentation_for(&self.lines[j])
                                            + self.lines[j].text.len()
                                            + 1,
                                    );
                                }
                            }
                        }
                    }

                    if l.text.is_empty() {
                        String::new()
                    } else {
                        if l.annotation.is_some() {
                            format!(
                                "{}{:w$}{}",
                                self.make_indent(l),
                                l.text,
                                l.annotation
                                    .as_ref()
                                    .map(|s| s.as_str())
                                    .unwrap_or_default(),
                                w = max_len - self.indentation_for(l)
                            )
                        } else {
                            format!("{}{}", self.make_indent(l), l.text,)
                        }
                    }
                })
                .join("\n")
        };

        *self = Self::new();

        r
    }

    pub fn depth(&self) -> usize {
        self.depths.len()
    }
}
