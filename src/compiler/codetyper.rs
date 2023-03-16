use colored::{Color, Colorize};

pub struct Tty {
    depths: Vec<usize>,
    o: Vec<String>,
}

impl Tty {
    pub fn new() -> Self {
        Self {
            depths: vec![0],
            o: vec![String::new()],
        }
    }

    pub fn write<S: AsRef<str>>(&mut self, l: S) {
        self.o.last_mut().unwrap().push_str(l.as_ref());
    }
    pub fn shift(&mut self, d: usize) {
        self.depths.push(d);
    }
    pub fn unshift(&mut self) {
        self.depths.pop();
    }
    pub fn cr(&mut self) {
        let indent = self
            .depths
            .iter()
            .skip(1)
            .map(|d| " ".repeat((*d as isize - 1).max(0) as usize))
            .collect::<Vec<_>>()
            .join("│".color(Color::BrightBlack).to_string().as_str());
        self.o.push(indent);
    }
    pub fn page_feed(&self) -> String {
        self.o.join("\n")
    }
    pub fn depth(&self) -> usize {
        self.depths.len()
    }
}
