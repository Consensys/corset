use crate::{
    column::Value,
    compiler::ColumnRef,
    inspect::{
        forth::{self, Node},
        StdTerminal,
    },
};
use ratatui::{
    prelude::Rect,
    style::{Color, Style},
    widgets::{Block, Borders},
};
use ratatui_textarea::{Input, Key, TextArea};
use std::collections::HashMap;

pub struct ScanInput<'a> {
    module: String,
    columns: &'a HashMap<String, ColumnRef>,
    input: TextArea<'a>,
}
impl<'a> ScanInput<'a> {
    pub fn new(module: &str, content: &str, columns: &'a HashMap<String, ColumnRef>) -> Self {
        let mut r = ScanInput {
            module: module.to_owned(),
            columns,
            input: TextArea::from([content]),
        };
        r.input.move_cursor(ratatui_textarea::CursorMove::End);
        r
    }

    fn validate(&mut self) -> anyhow::Result<Node> {
        let r = forth::parse(&self.input.lines()[0], &self.module, self.columns);

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

    pub fn run<F: Fn(isize, &ColumnRef) -> Option<Value>>(
        mut self,
        term: &mut StdTerminal,
        get: &F,
        max: isize,
        target: Rect,
    ) -> Option<(String, Vec<isize>)> {
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
                    return self
                        .validate()
                        .ok()
                        .map(|node| node.scan(get, max))
                        .map(|i| (self.input.into_lines()[0].to_owned(), i));
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
