//! Simple lexing of file content to support syntax highlighting
use crate::{config::ColorScheme, term::Style};
use std::cmp::min;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenType {
    Dot,
    Load,
    Execute,
    Default,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Token<'a> {
    pub ty: TokenType,
    pub s: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn render(&self, cs: &ColorScheme) -> String {
        match self.ty {
            TokenType::Dot => format!("{}{}", Style::Bg(cs.dot_bg), self.s),
            TokenType::Load => format!("{}{}", Style::Bg(cs.load_bg), self.s),
            TokenType::Execute => format!("{}{}", Style::Bg(cs.exec_bg), self.s),
            TokenType::Default => format!("{}{}{}", Style::Bg(cs.bg), Style::Fg(cs.fg), self.s),
        }
    }

    fn with_highlighted_dot(self, start: usize, end: usize, ty: TokenType) -> Vec<Token<'a>> {
        let (byte_start, _) = self.s.char_indices().nth(start).unwrap_or((0, 'a'));
        let (byte_end, _) = self.s.char_indices().nth(end).unwrap_or((usize::MAX, 'a'));
        let clamped_end = min(byte_end, self.s.len());
        let mut tks = Vec::with_capacity(3);

        if byte_start > 0 {
            tks.push(Token {
                ty: self.ty,
                s: &self.s[..byte_start],
            });
        }

        tks.push(Token {
            ty,
            s: &self.s[byte_start..clamped_end],
        });

        if clamped_end < self.s.len() {
            tks.push(Token {
                ty: self.ty,
                s: &self.s[clamped_end..],
            });
        }

        tks
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tokens<'a> {
    Single(Token<'a>),
    Multi(Vec<Token<'a>>),
}

impl<'a> Tokens<'a> {
    pub fn with_highlighted_dot(self, start: usize, end: usize, ty: TokenType) -> Vec<Token<'a>> {
        match self {
            Self::Single(tk) => tk.with_highlighted_dot(start, end, ty),

            Self::Multi(tks) => {
                let mut new_tks = Vec::new();
                let mut offset = 0;

                for tk in tks.into_iter() {
                    let len = tk.s.chars().count();
                    let token_end = offset + len;

                    if start >= token_end || end <= offset {
                        new_tks.push(tk);
                    } else {
                        new_tks.extend_from_slice(&tk.with_highlighted_dot(
                            start.saturating_sub(offset),
                            end - offset,
                            ty,
                        ));
                    }
                    offset += len;
                }

                new_tks
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn t_def(s: &str) -> Token<'_> {
        Token { ty: Default, s }
    }

    fn t_dot(s: &str) -> Token<'_> {
        Token { ty: Dot, s }
    }

    #[test]
    fn with_highlighted_dot_works_for_single_token() {
        let tk = t_def("hello, world!");
        let tks = tk.with_highlighted_dot(5, 9, TokenType::Dot);

        assert_eq!(tks, vec![t_def("hello"), t_dot(", wo"), t_def("rld!"),]);
    }

    #[test]
    fn with_highlighted_dot_works_with_multibyte() {
        let tks = vec![t_def("hello, world! [a-z¡-￿0-9_\\-./@]+")];
        let tks = Tokens::Multi(tks).with_highlighted_dot(18, 21, TokenType::Dot);

        assert_eq!(
            tks,
            vec![
                t_def("hello, world! [a-z"),
                t_dot("¡-￿"),
                t_def("0-9_\\-./@]+"),
            ]
        );
    }
}
