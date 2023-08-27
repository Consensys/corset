use crossterm::event;
use pairing_ce::bn256::Fr;
use ratatui::{
    prelude::Rect,
    style::{Color, Style},
    widgets::{Block, Borders},
};
use regex_lite::Regex;
use std::collections::HashMap;
use tui_textarea::{Input, Key, TextArea};

use crate::compiler::ColumnRef;

use super::{forth::Node, StdTerminal};

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

            match event::read().unwrap().into() {
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

            match event::read().unwrap().into() {
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

pub struct ForthInput<'a> {
    module: String,
    columns: &'a HashMap<String, ColumnRef>,
    input: TextArea<'a>,
}
impl<'a> ForthInput<'a> {
    pub fn new(module: &str, content: String, columns: &'a HashMap<String, ColumnRef>) -> Self {
        let mut r = ForthInput {
            module: module.to_owned(),
            columns,
            input: TextArea::from([content]),
        };
        r.input.move_cursor(tui_textarea::CursorMove::End);
        r
    }

    fn validate(&mut self) -> anyhow::Result<Node> {
        let r = super::forth::parse(&self.input.lines()[0], &self.module, self.columns);

        match &r {
            Err(err) => {
                self.input.set_style(Style::default().fg(Color::LightRed));
                self.input.set_block(
                    Block::default()
                        .borders(Borders::ALL)
                        .title(format!("ERROR: {}", err)),
                );
            }
            Ok(node) => {
                self.input.set_style(Style::default().fg(Color::LightGreen));
                self.input.set_block(
                    Block::default()
                        .borders(Borders::ALL)
                        .title(node.to_string()),
                );
            }
        };
        r
    }

    pub fn run<F: Fn(isize, &ColumnRef) -> Option<Fr>>(
        mut self,
        term: &mut StdTerminal,
        get: &F,
        max: isize,
        target: Rect,
    ) -> Option<isize> {
        self.input.set_cursor_line_style(Style::default());
        loop {
            let _ = self.validate();
            let _ = term.draw(|f| {
                f.render_widget(self.input.widget(), target);
            });

            match event::read().unwrap().into() {
                Input {
                    key: Key::Enter, ..
                } => {
                    let _ = term.clear();
                    return self.validate().ok().and_then(|node| node.scan(get, max));
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
