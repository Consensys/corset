use ratatui::{
    prelude::Rect,
    style::{Color, Style},
    widgets::{Block, Borders},
};
use tui_textarea::{Input, Key, TextArea};

use crate::inspect::StdTerminal;

pub struct NumberInput<'a> {
    title: String,
    input: TextArea<'a>,
}
impl NumberInput<'_> {
    pub fn new(title: &str) -> Self {
        NumberInput {
            title: title.to_owned(),
            input: TextArea::default(),
        }
    }
    fn validate(&mut self) -> Result<isize, std::num::ParseIntError> {
        let r = self.input.lines()[0].parse::<isize>();
        if let Err(ref err) = r {
            self.input.set_style(Style::default().fg(Color::LightRed));
            self.input.set_block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(format!("{} ERROR: {}", &self.title, err)),
            );
        } else {
            self.input.set_style(Style::default().fg(Color::LightGreen));
            self.input.set_block(
                Block::default()
                    .borders(Borders::ALL)
                    .title(self.title.clone()),
            );
        }
        r
    }

    pub fn run(mut self, term: &mut StdTerminal, target: Rect) -> Option<isize> {
        self.input.set_cursor_line_style(Style::default());
        loop {
            let _ = self.validate();
            let _ = term.draw(|f| {
                f.render_widget(self.input.widget(), target);
            });

            match crossterm::event::read().unwrap().into() {
                Input {
                    key: Key::Enter, ..
                } => {
                    let _ = term.clear();
                    return self.validate().ok();
                }
                Input { key: Key::Esc, .. } => {
                    let _ = term.clear();
                    return None;
                }
                input => {
                    self.input.input(input);
                }
            }
        }
    }
}
