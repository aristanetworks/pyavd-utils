// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::Emitter;
use super::states::EmitterProperties;
use super::states::ParseState;
use crate::error::ErrorKind;
use crate::event::Event;
use crate::event::ScalarStyle;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::span::IndentLevel;
use crate::span::Span;

impl<'input> Emitter<'input> {
    pub(super) fn parse_quoted_string_content(&mut self) -> (Cow<'input, str>, Option<Span>) {
        let start_span = self.current_span();
        let mut content = String::new();

        // Skip StringStart
        if self.peek_kind() == Some(TokenKind::StringStart) {
            let _ = self.take_current();
        }

        // Collect content until StringEnd
        loop {
            match self.peek_kind() {
                Some(TokenKind::StringContent) => {
                    let Some(()) = self.peek_with(|token, _| {
                        if let Token::StringContent(text) = token {
                            content.push_str(text);
                        } else {
                            debug_assert!(false, "peek_kind/string_content desynchronized");
                        }
                    }) else {
                        debug_assert!(false, "expected StringContent token");
                        break;
                    };
                    let _ = self.take_current();
                }
                Some(TokenKind::StringEnd) => {
                    let Some(end_span) = self.peek_with(|_, span| span) else {
                        debug_assert!(false, "expected StringEnd token");
                        break;
                    };
                    let _ = self.take_current();
                    let full_span = Span::new(start_span.start..end_span.end);
                    return (Cow::Owned(content), Some(full_span));
                }
                Some(_) => {
                    let _ = self.take_current();
                }
                None => break,
            }
        }
        (Cow::Owned(content), None)
    }

    pub(super) fn parse_quoted_scalar(
        &mut self,
        properties: EmitterProperties<'input>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        let start_span = self.current_span();
        let _ = self.take_current(); // consume StringStart

        // Try to optimize for single-content-token case (most common)
        // Check if we have: StringContent followed immediately by StringEnd.
        // Keep this on borrowed token access; `self.peek()` would clone the
        // common-case token and showed up in local profiles.
        let (single_token_value, first_content) = if let Some((content_cow, content_span)) = self
            .peek_with(|token, span| {
                if let Token::StringContent(content) = token {
                    Some((content.clone(), span))
                } else {
                    None
                }
            })
            .flatten()
        {
            let _ = self.take_current();

            if self.peek_kind() == Some(TokenKind::StringEnd) {
                (Some((content_cow, content_span)), None)
            } else {
                // Multi-line case: save the first content we already consumed.
                (None, Some(content_cow))
            }
        } else {
            (None, None)
        };

        if let Some((content_cow, content_span)) = single_token_value {
            // Fast path: single content token, zero-copy!
            let end_span = if self.peek_kind() == Some(TokenKind::StringEnd) {
                let span = self.peek_with(|_, span| span).unwrap_or(content_span);
                let _ = self.take_current();
                span
            } else {
                content_span
            };

            let style = match quote_style {
                crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
                crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
            };

            let full_span = Span::new(start_span.start..end_span.end);

            return Event::Scalar {
                style,
                value: content_cow,
                properties: properties.into_event_box(),
                span: full_span,
            };
        }

        let mut parts: Vec<Cow<'input, str>> = Vec::new();
        if let Some(first) = first_content {
            parts.push(first);
        }
        self.state_stack.push(ParseState::QuotedScalar {
            properties,
            quote_style,
            min_indent,
            start_span,
            parts,
            end_span: start_span,
            pending_newlines: 0,
            needs_trim: false,
        });
        self.process_state_stack()
            .unwrap_or_else(|| self.emit_null())
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "arguments and control flow mirror the resumable quoted-scalar state payload"
    )]
    pub(super) fn process_quoted_scalar_state(
        &mut self,
        properties: EmitterProperties<'input>,
        quote_style: crate::lexer::QuoteStyle,
        min_indent: IndentLevel,
        start_span: Span,
        mut parts: Vec<Cow<'input, str>>,
        mut end_span: Span,
        mut pending_newlines: usize,
        mut needs_trim: bool,
    ) -> Event<'input> {
        match self.peek_kind() {
            Some(TokenKind::StringContent) => {
                let Some((content, span)) = self
                    .peek_with(|token, span| {
                        if let Token::StringContent(content) = token {
                            Some((content.clone(), span))
                        } else {
                            debug_assert!(false, "peek_kind/string_content desynchronized");
                            None
                        }
                    })
                    .flatten()
                else {
                    debug_assert!(false, "expected StringContent token");
                    self.error(ErrorKind::UnterminatedString, start_span);
                    return self.emit_null();
                };
                if needs_trim {
                    if let Some(last) = parts.last_mut() {
                        match last {
                            Cow::Borrowed(text) => {
                                let trimmed = text.trim_end_matches(' ');
                                if trimmed.len() != text.len() {
                                    *last = Cow::Borrowed(trimmed);
                                }
                            }
                            Cow::Owned(text) => {
                                let new_len = text.trim_end_matches(' ').len();
                                text.truncate(new_len);
                            }
                        }
                    }
                    needs_trim = false;
                }

                if pending_newlines > 0 {
                    if pending_newlines == 1 {
                        parts.push(Cow::Borrowed(" "));
                    } else {
                        for _ in 1..pending_newlines {
                            parts.push(Cow::Borrowed("\n"));
                        }
                    }
                    pending_newlines = 0;
                }

                parts.push(content.clone());
                end_span = span;
                let _ = self.take_current();
                self.state_stack.push(ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
            Some(TokenKind::LineStart) => {
                let Some((indent, line_start_span)) = self
                    .peek_with(|token, span| {
                        if let Token::LineStart(indent) = token {
                            Some((*indent, span))
                        } else {
                            debug_assert!(false, "peek_kind/line_start desynchronized");
                            None
                        }
                    })
                    .flatten()
                else {
                    debug_assert!(false, "expected LineStart token");
                    self.error(ErrorKind::UnterminatedString, start_span);
                    return self.emit_null();
                };
                let is_content_line = self.peek_kind_nth(1) == Some(TokenKind::StringContent);
                if is_content_line && indent < min_indent {
                    self.error(
                        ErrorKind::InvalidIndentationContext {
                            expected: min_indent,
                            found: indent,
                        },
                        line_start_span,
                    );
                }

                needs_trim = true;
                pending_newlines += 1;
                end_span = line_start_span;
                let _ = self.take_current();
                self.state_stack.push(ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
            Some(TokenKind::StringEnd) => {
                let Some(span) = self.peek_with(|_, span| span) else {
                    debug_assert!(false, "expected StringEnd token");
                    self.error(ErrorKind::UnterminatedString, start_span);
                    return self.emit_null();
                };
                if needs_trim && let Some(last) = parts.last_mut() {
                    match last {
                        Cow::Borrowed(text) => {
                            let trimmed = text.trim_end_matches(' ');
                            if trimmed.len() != text.len() {
                                *last = Cow::Borrowed(trimmed);
                            }
                        }
                        Cow::Owned(text) => {
                            let new_len = text.trim_end_matches(' ').len();
                            text.truncate(new_len);
                        }
                    }
                }

                if pending_newlines == 1 {
                    parts.push(Cow::Borrowed(" "));
                } else if pending_newlines > 1 {
                    for _ in 1..pending_newlines {
                        parts.push(Cow::Borrowed("\n"));
                    }
                }
                end_span = span;
                let _ = self.take_current();
            }
            Some(_) | None => {
                self.error(ErrorKind::UnterminatedString, start_span);
            }
        }

        let value: String = parts.iter().map(AsRef::as_ref).collect();

        let style = match quote_style {
            crate::lexer::QuoteStyle::Single => ScalarStyle::SingleQuoted,
            crate::lexer::QuoteStyle::Double => ScalarStyle::DoubleQuoted,
        };

        let full_span = Span::new(start_span.start..end_span.end);

        Event::Scalar {
            style,
            value: Cow::Owned(value),
            properties: properties.into_event_box(),
            span: full_span,
        }
    }
}
