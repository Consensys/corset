use crate::{
    compiler::ColumnRef,
    inspect::{
        forth::{self, Node},
        StdTerminal,
    },
    structs::Field,
};
use ratatui::{
    prelude::Rect,
    style::{Color, Style},
    widgets::{Block, Borders},
};
use std::collections::HashMap;
use tui_textarea::{Input, Key, TextArea};

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
        r.input.move_cursor(tui_textarea::CursorMove::End);
        r
    }

    fn validate<F: Field>(&mut self) -> anyhow::Result<Node<F>> {
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

    pub fn run<F: Field, Func: Fn(isize, &ColumnRef) -> Option<F>>(
        mut self,
        term: &mut StdTerminal,
        get: &Func,
        max: isize,
        target: Rect,
    ) -> Option<(String, Vec<isize>)> {
        self.input.set_cursor_line_style(Style::default());
        loop {
            let _ = self.validate::<F>();
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
