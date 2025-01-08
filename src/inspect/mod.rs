use crate::{
    compiler::{ColumnRef, ConstraintSet},
    pretty::Pretty,
    structs::Handle,
};
use anyhow::{bail, Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use itertools::Itertools;
use ratatui::{prelude::*, widgets::*};
use ratatui::{TerminalOptions, Viewport};
use regex_lite::Regex;
use std::collections::HashMap;

type Backend = CrosstermBackend<std::io::Stdout>;
type Frame<'a> = ratatui::Frame<'a>;
type StdTerminal = Terminal<Backend>;

const CONTEXT: isize = 50;

mod forth;
mod widgets;

struct ModuleView {
    /// The name of the associated module
    name: String,
    /// A cache of this module columns
    columns: Vec<(ColumnRef, Handle)>,
    /// Current horizontal offset in the table view
    h_shift: isize,
    /// Current vertical offset in the table view
    v_shift: i16,
    /// Size of the longest column in the module
    size: isize,

    /// A list of regexps; only columns matching at least one of them will be displayed
    regexps: Vec<Regex>,
    /// The current indices of the subset of columns to show from `columns`
    to_show: Vec<usize>,

    /// If any, the latest used Forth/scan expression
    last_scan: String,

    /// If set, avoid low-constrast colors
    high_contrast: bool,

    /// if set, values are blanked in cells relating to inactive
    /// perspectives.
    blank_perspectives: bool,
}
impl ModuleView {
    fn from_cs(
        cs: &ConstraintSet,
        name: &str,
        high_contrast: bool,
        blank_perspectives: bool,
    ) -> ModuleView {
        let mut max_size = 0;
        let columns: Vec<(ColumnRef, Handle)> = cs
            .columns
            .iter_module(name)
            .sorted_by(|c, d| c.1.handle.cmp(&d.1.handle))
            .map(|c| {
                max_size = max_size.max(cs.columns.len(&c.0).unwrap_or_default());
                (c.0, c.1.handle.clone())
            })
            .collect();
        let currently_shown = (0..columns.len()).collect();
        ModuleView {
            name: name.to_owned(),
            columns,
            h_shift: 0,
            v_shift: 0,
            size: max_size as isize - 1,

            regexps: Vec::new(),
            to_show: currently_shown,

            last_scan: String::new(),
            high_contrast,
            blank_perspectives,
        }
    }

    fn goto(&mut self, i: isize) {
        self.h_shift = i.clamp(0, self.size);
    }

    fn left(&mut self, x: isize) {
        self.h_shift -= self.h_shift.min(x);
    }

    fn right(&mut self, x: isize) {
        self.h_shift = (self.h_shift + x).min(self.size);
    }

    fn up(&mut self, x: i16) {
        self.v_shift -= self.v_shift.min(x);
    }

    fn down(&mut self, x: i16) {
        self.v_shift = (self.v_shift + x).min(self.to_show.len() as i16 - 1);
    }

    fn home(&mut self) {
        self.h_shift = 0;
    }

    fn end(&mut self) {
        self.h_shift = self.size;
    }

    fn current_columns(&self) -> impl Iterator<Item = &(ColumnRef, Handle)> {
        self.to_show.iter().map(|i| &self.columns[*i])
    }

    fn filter(&mut self, regexps: Vec<Regex>) {
        self.regexps = regexps;
        self.to_show = self
            .columns
            .iter()
            .enumerate()
            .filter_map(|(i, (_, handle))| {
                if self.regexps.is_empty() || {
                    self.regexps
                        .iter()
                        .any(|regex| regex.is_match(&handle.to_string()))
                } {
                    Some(i)
                } else {
                    None
                }
            })
            .collect();
    }

    fn clear_filter(&mut self) {
        self.filter(Vec::new());
    }

    fn render(&self, cs: &ConstraintSet, f: &mut Frame, target: Rect) {
        let span = 0.max(self.h_shift)..(self.h_shift + CONTEXT).min(self.size) + 1;
        // max width for each column; defaults to 3
        let max_perspective_len = self
            .current_columns()
            .filter_map(|(_, h)| h.perspective.as_ref().map(|p| p.len()))
            .max()
            .unwrap_or_default();
        let mut maxes = vec![3; span.len() + 1];

        let active_white_value = if self.high_contrast {
            Color::White
        } else {
            Color::Gray
        };
        let dimmed_value = if self.high_contrast {
            Color::Gray
        } else {
            Color::DarkGray
        };

        let block = Block::new().borders(Borders::NONE);
        let rows = self
            .current_columns()
            .skip(self.v_shift as usize)
            .map(|(column_ref, h)| {
                maxes[0] = maxes[0].max(h.name.len() + 1 + max_perspective_len);
                Row::new(
                    std::iter::once(
                        Cell::from(format!(
                            "{:width$} {}",
                            if let Some(p) = h.perspective.as_ref() {
                                p
                            } else {
                                ""
                            },
                            h.name.to_owned(),
                            width = max_perspective_len,
                        ))
                        .style(Style::default().blue().bold()),
                    )
                    .chain(span.clone().enumerate().map(|(k, i)| {
                        cs.columns
                            .get(column_ref, i, false)
                            .map(|x| {
                                let base = cs.columns.column(column_ref).unwrap().base;
                                let mut x_str = x.pretty_with_base(base);
                                // map color to the 231-17 range of readable color
                                // https://i.stack.imgur.com/KTSQa.png
                                let hash =
                                    x.to_bytes().iter().fold(0u8, |ax, bx| ax.wrapping_add(*bx));
                                let bg_color = (hash % (231 - 16)) + 0;
                                // ensure that we write white on dark colors and white on dark ones
                                let corrected_fg_color = if bg_color % 36 > 18 {
                                    Color::Black
                                } else if bg_color == 0 {
                                    active_white_value
                                } else {
                                    Color::White
                                };

                                // dim the column if its perspective is inactive
                                let dim = if let Some(perspective) =
                                    cs.columns.perspective(column_ref).unwrap()
                                {
                                    cs.get_perspective(&h.module, perspective)
                                        .unwrap()
                                        .eval(
                                            i,
                                            |handle, i, wrap| cs.columns.get_raw(handle, i, wrap),
                                            &mut None,
                                            &Default::default(),
                                        )
                                        .map(|x| x.is_zero())
                                        .unwrap_or(false)
                                } else {
                                    false
                                };
                                // Blank out the column if its not in
                                // an active perspective.
                                if dim && self.blank_perspectives {
                                    x_str = String::new();
                                }
                                // Update column widths
                                maxes[k + 1] = maxes[k + 1].max(x_str.len());
                                // render the cell
                                if dim {
                                    Cell::from(x_str).fg(dimmed_value).bg(Color::Reset)
                                } else {
                                    let bg_col = if bg_color > 0 {
                                        Color::Indexed(bg_color.wrapping_add(16) % 251)
                                    } else {
                                        Color::Reset
                                    };
                                    //
                                    Cell::from(x_str).fg(corrected_fg_color).bg(bg_col)
                                }
                            })
                            .unwrap_or(Cell::from("."))
                    })),
                )
                .style(Style::default().white())
            })
            .collect::<Vec<_>>();
        let widths = maxes
            .iter()
            .map(|w| Constraint::Min(*w as u16))
            .collect::<Vec<_>>();

        let table = Table::new(rows, widths)
            .header(
                Row::new(std::iter::once(String::new()).chain(span.map(|i| i.to_string())))
                    .style(Style::default().bold().blue()),
            )
            .flex(layout::Flex::Legacy)
            .block(block);
        f.render_widget(table, target);
    }
}

struct Inspector<'a> {
    cs: &'a ConstraintSet,
    modules: Vec<ModuleView>,
    current_module: usize,
    tab_offset: usize,
    minibuffer: Rect,
    message: Span<'a>,
}
impl<'a> Inspector<'a> {
    fn from_cs(
        cs: &'a ConstraintSet,
        high_contrast: bool,
        blank_perspectives: bool,
    ) -> Result<Self> {
        let r = Inspector {
            cs,
            modules: cs
                .columns
                .modules()
                .iter()
                .map(|n| ModuleView::from_cs(cs, n, high_contrast, blank_perspectives))
                .sorted_by(|m1, m2| m1.name.cmp(&m2.name))
                .collect(),
            current_module: 0,
            tab_offset: 0,
            minibuffer: Default::default(),
            message: Span::from(""),
        };
        if r.modules.is_empty() {
            bail!("no modules found in provided constraint system");
        } else {
            Ok(r)
        }
    }

    fn open_module(&mut self, module: &str) {
        self.current_module = self
            .modules
            .iter()
            .enumerate()
            .find(|(_, m)| m.name == module)
            .map(|(i, _)| i)
            .unwrap_or_default();
    }

    fn current_module(&self) -> &ModuleView {
        self.modules.get(self.current_module).unwrap()
    }

    fn current_module_mut(&mut self) -> &mut ModuleView {
        self.modules.get_mut(self.current_module).unwrap()
    }

    fn render_tabs(&mut self, f: &mut Frame, place: Rect) {
        let mut titles = self
            .modules
            .iter()
            .map(|t| t.name.clone())
            .collect::<Vec<_>>();
        // Determine how much space
        let tab_offset = self.update_tab_offset(place.width as usize, &titles);
        // Slice out hidden tabs
        titles.drain(0..tab_offset);
        //
        let tabs = Tabs::new(titles)
            .block(
                Block::default()
                    .borders(Borders::BOTTOM)
                    .title(Line::from(vec![
                        "← ".into(),
                        "[S-TAB]".yellow().bold(),
                        " Modules ".into(),
                        "[TAB]".yellow().bold(),
                        " →".into(),
                    ])),
            )
            .select(self.current_module - tab_offset)
            // .style(Style::default().dark_gray())
            .highlight_style(
                Style::default()
                    .white()
                    .add_modifier(Modifier::BOLD)
                    .add_modifier(Modifier::UNDERLINED),
            );
        f.render_widget(tabs, place);
    }

    fn update_tab_offset(&mut self, width: usize, titles: &[String]) -> usize {
        let mut count = 10;
        loop {
            let (start, end) = self.determine_selected_offset(titles);
            if end >= (width as isize) {
                self.tab_offset += 1;
            } else if start < 0 {
                self.tab_offset -= 1;
            } else if count == 0 {
                // this just ensures we can never enter an infinite loop.
                return 0;
            } else {
                return self.tab_offset;
            }
            count -= 1;
        }
    }

    fn determine_selected_offset(&self, titles: &[String]) -> (isize, isize) {
        let mut offset = 0isize;
        for t in 0..self.current_module {
            offset += (titles[t].len() + 3) as isize;
        }
        for t in 0..self.tab_offset {
            offset -= (titles[t].len() + 3) as isize;
        }
        // Done
        (
            offset,
            offset + (titles[self.current_module].len() as isize),
        )
    }

    fn render_columns(&self, f: &mut Frame, target: Rect) {
        self.current_module().render(self.cs, f, target);
    }

    fn render_help(&self, f: &mut Frame) {
        let titles = vec![
            "[g]".yellow().bold(),
            "oto".into(),
            " :: ".dark_gray(),
            "[f]".yellow().bold(),
            "ilter".into(),
            " :: ".dark_gray(),
            "[#]".yellow().bold(),
            "clear filter".into(),
            " :: ".dark_gray(),
            "[s]".yellow().bold(),
            "can".into(),
            " :: ".dark_gray(),
            // "[p]".yellow().bold(),
            // "lookup".into(),
            // " :: ".into(),
            "[p]".yellow().bold(),
            "erspectives".into(),
            " :: ".into(),
            "[q]".red().bold(),
            "uit".into(),
        ];
        f.render_widget(
            Paragraph::new(vec![Line::from(titles), Line::from(self.message.clone())])
                .block(Block::default().title("Commands").borders(Borders::TOP)),
            self.minibuffer,
        );
    }

    fn render(&mut self, f: &mut Frame) {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Length(3),
                    Constraint::Min(5),
                    Constraint::Length(3),
                ]
                .as_ref(),
            )
            .split(size);
        self.minibuffer = chunks[2];

        let block = Block::default();
        f.render_widget(block, size);
        self.render_tabs(f, chunks[0]);
        self.render_columns(f, chunks[1]);
        self.render_help(f);
    }

    fn prev(&mut self) {
        self.current_module = if self.current_module == 0 {
            self.modules.len() - 1
        } else {
            self.current_module - 1
        }
    }

    fn next(&mut self) {
        self.current_module = (self.current_module + 1) % self.modules.len();
    }

    fn run(&mut self, terminal: &mut StdTerminal, _settings: InspectorSettings) -> Result<()> {
        loop {
            terminal.draw(|term| self.render(term))?;
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') => return Ok(()),
                        KeyCode::Char('s') => {
                            let mut t = Terminal::with_options(
                                CrosstermBackend::new(std::io::stdout()),
                                TerminalOptions {
                                    viewport: Viewport::Fixed(self.minibuffer),
                                },
                            )
                            .unwrap();
                            let column_cache = self
                                .current_module()
                                .columns
                                .iter()
                                .map(|(r, h)| (h.name.clone(), r.clone()))
                                .collect::<HashMap<_, _>>();
                            let is = widgets::scan::ScanInput::new(
                                &self.current_module().name,
                                &self.current_module().last_scan,
                                &column_cache,
                            )
                            .run(
                                &mut t,
                                &|i, r| self.cs.columns.get_raw(r, i, false),
                                self.current_module().size,
                                self.minibuffer,
                            );
                            if let Some((exp, is)) = is {
                                self.current_module_mut().last_scan = exp.clone();
                                if is.is_empty() {
                                    self.message = "Not found".red();
                                } else {
                                    self.message = Span::from(format!(
                                        "'{}' found at {}",
                                        exp,
                                        is.iter().join(" ")
                                    ))
                                    .green();
                                    self.current_module_mut().goto(is[0]);
                                }
                            }
                            let _ = terminal.clear();
                        }
                        KeyCode::Char('g') => {
                            let mut t = Terminal::with_options(
                                CrosstermBackend::new(std::io::stdout()),
                                TerminalOptions {
                                    viewport: Viewport::Fixed(self.minibuffer),
                                },
                            )
                            .unwrap();
                            let i = widgets::number::NumberInput::new("Go to column...")
                                .run(&mut t, self.minibuffer);
                            if let Some(i) = i {
                                self.current_module_mut().goto(i);
                            }
                            let _ = terminal.clear();
                        }
                        KeyCode::Char('f') => {
                            let mut t = Terminal::with_options(
                                CrosstermBackend::new(std::io::stdout()),
                                TerminalOptions {
                                    viewport: Viewport::Fixed(self.minibuffer),
                                },
                            )
                            .unwrap();
                            let regexs = widgets::regexp::RegexpInput::new(
                                "Filter columns matching",
                                self.current_module()
                                    .regexps
                                    .iter()
                                    .map(|regexp| regexp.to_string())
                                    .join(" "),
                            )
                            .run(&mut t, self.minibuffer);
                            if let Some(regexs) = regexs {
                                self.current_module_mut().filter(regexs);
                            }
                            let _ = terminal.clear();
                        }
                        KeyCode::Char('#') => self.current_module_mut().clear_filter(),
                        KeyCode::Char('p') => {
                            let flag = self.current_module().blank_perspectives;
                            self.current_module_mut().blank_perspectives = !flag;
                            let _ = terminal.clear();
                        }
                        KeyCode::BackTab => {
                            self.prev();
                        }
                        KeyCode::Tab => {
                            if key.modifiers == KeyModifiers::SHIFT {
                                self.prev();
                            } else {
                                self.next();
                            }
                        }
                        KeyCode::Left => {
                            self.current_module_mut().left(1);
                        }
                        KeyCode::Right => {
                            self.current_module_mut().right(1);
                        }
                        KeyCode::Up => {
                            self.current_module_mut().up(1);
                        }
                        KeyCode::Down => {
                            self.current_module_mut().down(1);
                        }
                        KeyCode::PageUp => {
                            if key.modifiers.contains(KeyModifiers::SHIFT) {
                                self.current_module_mut().left(1000);
                            } else {
                                self.current_module_mut().left(100);
                            }
                        }
                        KeyCode::PageDown => {
                            if key.modifiers.contains(KeyModifiers::SHIFT) {
                                self.current_module_mut().right(1000);
                            } else {
                                self.current_module_mut().right(100);
                            }
                        }
                        KeyCode::Home => {
                            self.current_module_mut().home();
                        }
                        KeyCode::End => {
                            self.current_module_mut().end();
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}

pub(crate) struct InspectorSettings {
    pub open_module: Option<String>,
    pub high_contrast: bool,
    pub blank_perspectives: bool,
}

pub(crate) fn inspect(cs: &ConstraintSet, settings: InspectorSettings) -> Result<()> {
    let mut inspector =
        Inspector::from_cs(cs, settings.high_contrast, settings.blank_perspectives)?;
    if let Some(module) = settings.open_module.as_ref() {
        inspector.open_module(module);
    }
    let mut terminal = setup_terminal()?;
    inspector.run(&mut terminal, settings)?;
    restore_terminal(&mut terminal)?;
    Ok(())
}

fn setup_terminal() -> Result<StdTerminal> {
    let mut stdout = std::io::stdout();
    enable_raw_mode().context("failed to enable raw mode")?;
    execute!(stdout, EnterAlternateScreen).context("unable to enter alternate screen")?;
    Terminal::new(CrosstermBackend::new(stdout)).context("creating terminal failed")
}

fn restore_terminal(terminal: &mut StdTerminal) -> Result<()> {
    disable_raw_mode().context("failed to disable raw mode")?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)
        .context("unable to switch to main screen")?;
    terminal.show_cursor().context("unable to show cursor")
}
