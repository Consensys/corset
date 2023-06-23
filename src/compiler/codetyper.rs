use itertools::Itertools;
use owo_colors::{colored::Color, OwoColorize};

pub struct Tty {
    with_guides: bool,

    depths: Vec<usize>,
    o: Vec<String>,
    end_line: Option<String>,
}

impl Tty {
    pub fn new() -> Self {
        Self {
            with_guides: false,
            depths: vec![0],
            o: vec![String::new()],
            end_line: None,
        }
    }
    pub fn with_guides(self) -> Self {
        Tty {
            with_guides: true,
            ..self
        }
    }
    pub fn write<S: AsRef<str>>(&mut self, l: S) {
        self.o.last_mut().unwrap().push_str(l.as_ref());
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
        self.end_line = Some(s);
    }
    pub fn cr(&mut self) {
        if let Some(ending) = self.end_line.as_ref() {
            self.o.last_mut().unwrap().push_str(ending);
            self.end_line = None;
        }
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
        self.o.push(indent);
    }
    pub fn page_feed(&self) -> String {
        self.o.join("\n")
    }
    pub fn depth(&self) -> usize {
        self.depths.len()
    }
}
