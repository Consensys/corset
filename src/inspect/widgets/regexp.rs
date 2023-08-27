use ratatui::{
    prelude::Rect,
    style::{Color, Style},
    widgets::{Block, Borders},
};
use regex_lite::Regex;
use tui_textarea::{Input, Key, TextArea};

use crate::inspect::StdTerminal;

pub struct RegexpInput<'a> {
    title: String,
    input: TextArea<'a>,
}
impl RegexpInput<'_> {
    pub fn new(title: &str, content: String) -> Self {
        let mut r = RegexpInput {
            title: title.to_owned(),
            input: TextArea::from([content]),
        };
        r.input.move_cursor(tui_textarea::CursorMove::End);
        r
    }

    fn validate(&mut self) -> Result<Vec<Regex>, regex_lite::Error> {
        let r = self.input.lines()[0]
            .split_whitespace()
            .into_iter()
            .map(|s| Regex::new(s))
            .collect::<Result<Vec<_>, _>>();

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

    pub fn run(mut self, term: &mut StdTerminal, target: Rect) -> Option<Vec<Regex>> {
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
