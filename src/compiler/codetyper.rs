use itertools::Itertools;
use owo_colors::{colored::Color, OwoColorize};

pub struct Tty {
    with_guides: bool,
    align_annotations: bool,

    depths: Vec<usize>,
    o: Vec<(String, Option<String>)>,
}

impl Tty {
    pub fn new() -> Self {
        Self {
            with_guides: false,
            align_annotations: false,
            depths: vec![0],
            o: vec![(String::new(), None)],
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
    pub fn write<S: AsRef<str>>(&mut self, l: S) {
        let l = l.as_ref();
        if l.contains('\n') {
            panic!("newlines found in `{}`", l);
        }
        self.o.last_mut().unwrap().0.push_str(l);
    }
    pub fn shift(&mut self, d: usize) {
        self.depths.push(d);
    }
    pub fn indentation(&self) -> usize {
        self.depths.iter().sum()
    }
    pub fn unshift(&mut self) {
        self.depths.pop();
    }
    pub fn buffer_end(&mut self, s: String) {
        if let Some(end) = self.o.last_mut().and_then(|l| l.1.as_mut()) {
            end.push_str(&s)
        } else {
            self.o.last_mut().unwrap().1 = Some(s)
        }
    }
    pub fn cr(&mut self) {
        let indent = if self.with_guides {
            " ".to_string() // Account for the skipped first '|'
            + &self
                .depths
                .iter()
                .skip(1)
                .map(|d| " ".repeat((*d as isize - 1).max(0) as usize))
                .collect::<Vec<_>>()
                .join("│".color(Color::BrightBlack).to_string().as_str())
        } else {
            " ".repeat(self.indentation())
        };
        self.o.push((indent, None));
    }
    pub fn page_feed(&mut self) -> String {
        let r = if !self.align_annotations {
            self.o
                .iter()
                .map(|(l, a)| {
                    format!(
                        "{}{}",
                        l,
                        a.as_ref().map(|s| s.as_str()).unwrap_or_default()
                    )
                })
                .join("\n")
        } else {
            let max_len = if self.o.len() > 1 {
                self.o
                    .iter()
                    .filter(|l| l.1.is_some())
                    .map(|l| l.0.len())
                    .max()
                    .unwrap_or_default()
                    + 1
            } else {
                0
            };

            self.o
                .iter()
                .map(|(l, a)| {
                    if l.is_empty() {
                        String::new()
                    } else {
                        format!(
                            "{:w$}{}",
                            l,
                            a.as_ref().map(|s| s.as_str()).unwrap_or_default(),
                            w = max_len
                        )
                    }
                })
                .join("\n")
        };

        self.depths = vec![0];
        self.o = vec![Default::default()];

        r
    }
    pub fn depth(&self) -> usize {
        self.depths.len()
    }
}
