// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Legacy YAML lexer (tokenizer) with error recovery.
//!
//! This module implements a chumsky-based lexer that doesn't track flow context.
//! It is kept for comparison and benchmarking purposes.
//!
//! For production use, prefer the context-aware lexer in `context_lexer.rs`.
//!
//! Token types are defined in `token.rs` and re-exported here for backwards compatibility.

use chumsky::prelude::*;

use crate::span::{Span, Spanned};

// Re-export token types from the token module for backwards compatibility
pub use crate::token::{BlockScalarHeader, Chomping, QuoteStyle, Token};

/// Create the YAML lexer parser.
///
/// This returns a chumsky parser that tokenizes YAML input with error recovery.
#[allow(
    clippy::too_many_lines,
    reason = "Complex parser combinator logic, will be refactored later"
)]
fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token>>, extra::Err<Rich<'src, char, Span>>> {
    // === Character-level primitives ===

    // Newline characters (YAML 1.2: b-break)
    let newline = just('\n')
        .or(just('\r').then(just('\n').or_not()).ignored().to('\n'))
        .or(just('\u{0085}')) // NEL
        .or(just('\u{2028}')) // Line separator
        .or(just('\u{2029}')); // Paragraph separator

    // Single space (YAML doesn't allow tabs for indentation)
    let space = just(' ');

    // Whitespace: spaces and tabs (for inline use, not indentation)
    let inline_ws = one_of(" \t").repeated().at_least(1).ignored();

    // Comment: # followed by content until end of line
    let comment = just('#')
        .ignore_then(
            none_of("\n\r\u{0085}\u{2028}\u{2029}")
                .repeated()
                .to_slice(),
        )
        .map(|string: &str| Token::Comment(string.to_owned()));

    // Line start: newline followed by indentation (spaces only)
    let line_start = newline.ignore_then(space.repeated().count().map(Token::LineStart));

    // === Flow indicators ===
    let flow_indicator = choice((
        just('{').to(Token::FlowMapStart),
        just('}').to(Token::FlowMapEnd),
        just('[').to(Token::FlowSeqStart),
        just(']').to(Token::FlowSeqEnd),
        just(',').to(Token::Comma),
    ));

    // === Document markers ===
    // Must be at start of line (handled by context) and followed by whitespace/newline/EOF
    let doc_start = just("---")
        .then(one_of(" \t\n\r").rewind().or(end().to(' ')))
        .to(Token::DocStart);
    let doc_end = just("...")
        .then(one_of(" \t\n\r").rewind().or(end().to(' ')))
        .to(Token::DocEnd);

    // === Block structure indicators ===
    // These indicators are only special when followed by whitespace or newline
    let block_seq = just('-')
        .then(one_of(" \t\n\r").rewind().or(end().to(' ')))
        .to(Token::BlockSeqIndicator);
    let mapping_key = just('?')
        .then(one_of(" \t\n\r").rewind().or(end().to(' ')))
        .to(Token::MappingKey);

    // Colon: mapping value indicator
    // In block context, must be followed by whitespace/newline
    // In flow context, can be followed by flow indicators too
    // For now, we'll be conservative and require whitespace
    let colon = just(':')
        .then(one_of(" \t\n\r,}]").rewind().or(end().to(' ')))
        .to(Token::Colon);

    // === Anchors and Aliases ===
    // Anchor: &name where name is ns-anchor-char+
    // Alias: *name
    let anchor_char = any().filter(|ch: &char| ch.is_alphanumeric() || *ch == '-' || *ch == '_');
    let anchor_name = anchor_char.repeated().at_least(1).to_slice();

    let anchor = just('&')
        .ignore_then(anchor_name)
        .map(|string: &str| Token::Anchor(string.to_owned()));

    let alias = just('*')
        .ignore_then(anchor_name)
        .map(|string: &str| Token::Alias(string.to_owned()));

    // === Tags ===
    // Verbatim tag: !<uri>
    // Shorthand tag: !handle!suffix or !suffix
    // Non-specific tag: ! or !!
    let tag_char = any().filter(|ch: &char| {
        ch.is_alphanumeric() || matches!(ch, '-' | '_' | '.' | '/' | ':' | '#' | '%')
    });

    let tag = just('!')
        .ignore_then(
            // Verbatim tag: !<...>
            just('<')
                .ignore_then(none_of(">").repeated().to_slice())
                .then_ignore(just('>'))
                .map(|string: &str| format!("!<{string}>"))
                // Named tag handle: !!type or !handle!type or !type
                .or(just('!')
                    .ignore_then(tag_char.repeated().to_slice())
                    .map(|string: &str| format!("!!{string}")))
                // Primary tag: ! alone or !suffix
                .or(tag_char.repeated().to_slice().map(|string: &str| {
                    if string.is_empty() {
                        "!".to_owned()
                    } else {
                        format!("!{string}")
                    }
                })),
        )
        .map(Token::Tag);

    // === Quoted scalars ===

    // Double-quoted string with escape sequences
    let double_escape = just('\\').ignore_then(choice((
        just('\\').to('\\'),
        just('"').to('"'),
        just('/').to('/'),
        just('0').to('\0'),
        just('a').to('\x07'),
        just('b').to('\x08'),
        just('t').to('\t'),
        just('n').to('\n'),
        just('v').to('\x0B'),
        just('f').to('\x0C'),
        just('r').to('\r'),
        just('e').to('\x1B'),
        just(' ').to(' '),
        just('N').to('\u{0085}'), // NEL
        just('_').to('\u{00A0}'), // NBSP
        just('L').to('\u{2028}'), // Line separator
        just('P').to('\u{2029}'), // Paragraph separator
        // Unicode escapes: \xNN, \uNNNN, \UNNNNNNNN
        just('x')
            .ignore_then(text::digits(16).exactly(2).to_slice())
            .validate(|string: &str, extra, emitter| {
                u32::from_str_radix(string, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(extra.span(), "invalid \\x escape"));
                        '\u{FFFD}'
                    })
            }),
        just('u')
            .ignore_then(text::digits(16).exactly(4).to_slice())
            .validate(|string: &str, extra, emitter| {
                u32::from_str_radix(string, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(extra.span(), "invalid \\u escape"));
                        '\u{FFFD}'
                    })
            }),
        just('U')
            .ignore_then(text::digits(16).exactly(8).to_slice())
            .validate(|string: &str, extra, emitter| {
                u32::from_str_radix(string, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(extra.span(), "invalid \\U escape"));
                        '\u{FFFD}'
                    })
            }),
        // Line continuation: backslash at end of line
        newline.then(space.repeated()).to(' '), // Escaped newline becomes space
    )));

    // NOTE: The chumsky lexer produces simplified StringContent tokens.
    // The context_lexer produces proper StringStart/StringContent/StringEnd tokens.
    let double_quoted = none_of("\\\"")
        .or(double_escape)
        .repeated()
        .collect::<String>()
        .delimited_by(just('"'), just('"'))
        .map(Token::StringContent) // Simplified for chumsky lexer
        .recover_with(via_parser(
            just('"')
                .ignore_then(none_of("\"").repeated())
                .then_ignore(end().rewind().or(just('"').ignored()))
                .to(Token::Invalid),
        ));

    // Single-quoted string (only escape is '' for literal ')
    let single_char = none_of("'").or(just("''").to('\''));
    let single_quoted = single_char
        .repeated()
        .collect::<String>()
        .delimited_by(just('\''), just('\''))
        .map(Token::StringContent) // Simplified for chumsky lexer
        .recover_with(via_parser(
            just('\'')
                .ignore_then(none_of("'").repeated())
                .then_ignore(end().rewind().or(just('\'').ignored()))
                .to(Token::Invalid),
        ));

    // === Block scalar headers ===
    // | or > followed by optional indentation indicator and chomping indicator
    let chomping_choice = choice((
        just('-').to(Chomping::Strip),
        just('+').to(Chomping::Keep),
        empty().to(Chomping::Clip),
    ));

    let indent_indicator = one_of("123456789")
        .map(|ch: char| {
            // to_digit(10) returns 0-9 for valid digits, which always fits in u8
            ch.to_digit(10).and_then(|digit| u8::try_from(digit).ok())
        })
        .or(empty().to(None));

    let block_header = indent_indicator
        .then(chomping_choice)
        .or(chomping_choice.map(|chomping| (None, chomping)));

    let literal_header = just('|')
        .ignore_then(block_header)
        .map(|(indent, chomping)| {
            Token::LiteralBlockHeader(BlockScalarHeader { indent, chomping })
        });

    let folded_header = just('>')
        .ignore_then(block_header)
        .map(|(indent, chomping)| Token::FoldedBlockHeader(BlockScalarHeader { indent, chomping }));

    // === Directives ===
    let directive_content = none_of("\n\r\u{0085}\u{2028}\u{2029}")
        .repeated()
        .to_slice();
    let yaml_directive = just("%YAML")
        .ignore_then(directive_content)
        .map(|string: &str| Token::YamlDirective(string.trim().to_owned()));
    let tag_directive = just("%TAG")
        .ignore_then(directive_content)
        .map(|string: &str| Token::TagDirective(string.trim().to_owned()));
    // Reserved/unknown directives: %NAME where NAME is not YAML or TAG
    let reserved_directive = just('%')
        .ignore_then(directive_content)
        .map(|string: &str| Token::ReservedDirective(string.trim().to_owned()));

    // === Plain scalars ===
    // Plain scalars are complex - they:
    // - Can't start with most indicators (except -/:/? if followed by non-space)
    // - Can't contain `: ` (colon+space) or ` #` (space+hash)
    // - Stop at flow indicators, newlines, and other structural characters

    // Characters that can never start a plain scalar
    // Note: * and & are handled specially below (they can start plain scalars when not valid alias/anchor)
    let indicator_chars = ",[]{}#!|>'\"%@`";

    // First character of plain scalar:
    // - Regular chars (non-indicator, non-whitespace)
    // - OR -/:/? when followed by non-whitespace (forms like ?foo, -foo, :foo)
    // - OR * when NOT followed by a valid anchor char (so it's not an alias)
    // - OR & when NOT followed by a valid anchor char (so it's not an anchor)
    let plain_regular_first = any().filter(move |ch: &char| {
        !indicator_chars.contains(*ch) && !"-?:*&".contains(*ch) && !ch.is_whitespace()
    });

    // -/:/? can start a plain scalar when followed by non-whitespace
    let plain_indicator_first = one_of("-?:")
        .then(none_of(" \t\n\r,[]{}").rewind())
        .map(|(ch, _)| ch);

    // * can start a plain scalar when NOT followed by a valid anchor char
    // (i.e., when it's not a valid alias)
    let anchor_char_set = |ch: char| ch.is_alphanumeric() || ch == '-' || ch == '_';
    let plain_asterisk = just('*')
        .then(
            any()
                .filter(move |ch: &char| !anchor_char_set(*ch))
                .rewind(),
        )
        .map(|(ch, _)| ch);

    // & can start a plain scalar when NOT followed by a valid anchor char
    let plain_ampersand = just('&')
        .then(
            any()
                .filter(move |ch: &char| !anchor_char_set(*ch))
                .rewind(),
        )
        .map(|(ch, _)| ch);

    // Subsequent characters in a plain scalar (we handle termination below)
    // A plain scalar continues until we hit:
    // - Flow indicators: , [ ] { }
    // - Newline characters
    // - `: ` sequence (colon followed by space)
    // - ` #` sequence (space followed by hash - comment)
    // We use a character-by-character approach and stop early

    // For simplicity, we'll match characters that are definitely part of the plain scalar:
    // - Not a flow indicator (these terminate plain scalars in both flow and block context for simplicity)
    // - Not a newline
    // - Not a colon (we'll handle colon specially)
    // - Not a hash (we'll handle hash specially)
    let plain_safe_char = none_of(",[]{}:\n\r\u{0085}\u{2028}\u{2029}# \t");

    // A colon is OK if NOT followed by whitespace or flow indicator
    let plain_colon = just(':').then(none_of(" \t\n\r,[]{}").rewind()).to(':');

    // A space/tab is OK if NOT followed by hash and NOT preceded by colon
    // This is tricky - we'll include spaces that are part of the value
    let plain_space = just(' ').then(none_of("#").rewind()).to(' ');
    let plain_tab = just('\t').then(none_of("#").rewind()).to('\t');

    // A hash is OK if NOT preceded by whitespace (but we've already consumed the char)
    // Since we handle space+hash by not consuming, hash alone can be part of plain scalar
    // Actually, in YAML `#` alone without preceding space is part of plain scalar
    let plain_hash = just('#');

    // Combine into a single "continue" character
    let plain_continue = choice((
        plain_safe_char,
        plain_colon,
        plain_space,
        plain_tab,
        plain_hash,
    ));

    let plain_first = choice((
        plain_regular_first,
        plain_indicator_first,
        plain_asterisk,
        plain_ampersand,
    ));

    let plain_scalar = plain_first
        .then(plain_continue.repeated().collect::<String>())
        .map(|(first, rest): (char, String)| {
            let mut string = String::with_capacity(1 + rest.len());
            string.push(first);
            string.push_str(&rest);
            Token::Plain(string.trim_end().to_owned())
        });

    // === Whitespace token ===
    let whitespace = inline_ws.to(Token::Whitespace);

    // === Main token parser ===
    // Order matters! More specific patterns first.
    let token = choice((
        // Document markers (must come before block_seq which starts with -)
        doc_start,
        doc_end,
        // Directives (order matters: YAML and TAG before reserved)
        yaml_directive,
        tag_directive,
        reserved_directive,
        // Block indicators
        block_seq,
        mapping_key,
        colon,
        // Flow indicators
        flow_indicator,
        // Anchors and aliases
        anchor,
        alias,
        // Tags
        tag,
        // Block scalar headers
        literal_header,
        folded_header,
        // Quoted scalars
        double_quoted,
        single_quoted,
        // Comments
        comment,
        // Line start with indentation
        line_start,
        // Whitespace
        whitespace,
        // Plain scalars (must be last as it's the catch-all)
        plain_scalar,
    ));

    // Handle initial indentation at file start
    let initial_indent = space
        .repeated()
        .count()
        .map(Token::LineStart)
        .map_with(|tok, extra| (tok, extra.span()));

    // Main token loop with error recovery
    let tokens = token
        .map_with(|tok, extra| (tok, extra.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect::<Vec<_>>();

    // Combine initial indent with rest of tokens
    initial_indent.then(tokens).map(|(first, mut rest)| {
        rest.insert(0, first);
        rest
    })
}

/// Tokenize YAML input.
///
/// Returns a list of tokens with spans and any errors encountered.
/// Due to error recovery, the function may return both tokens and errors.
pub fn tokenize(input: &str) -> (Vec<Spanned<Token>>, Vec<crate::error::ParseError>) {
    use crate::error::{ErrorKind, ParseError};
    use chumsky::span::Span as _;

    let (tokens, errs) = lexer().parse(input).into_output_errors();

    let parse_errors: Vec<ParseError> = errs
        .into_iter()
        .map(|err| {
            let span = err.span();
            ParseError {
                kind: ErrorKind::UnexpectedToken,
                span: Span::new((), span.start..span.end),
                expected: err.expected().map(|exp| format!("{exp:?}")).collect(),
                found: err.found().map(std::string::ToString::to_string),
            }
        })
        .collect();

    (tokens.unwrap_or_default(), parse_errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_types() {
        assert!(Token::Plain("hello".into()).is_scalar());
        // StringContent is not a scalar by itself - it's a component
        assert!(!Token::StringContent("world".into()).is_scalar());
        assert!(Token::FlowMapStart.is_flow_indicator());
        assert!(!Token::Colon.is_flow_indicator());
    }

    #[test]
    fn test_tokenize_empty() {
        let (tokens, errors) = tokenize("");
        assert!(errors.is_empty());
        // Should have initial LineStart(0)
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens.first().unwrap().0, Token::LineStart(0)));
    }

    #[test]
    fn test_tokenize_simple_mapping() {
        let (tokens, errors) = tokenize("key: value");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        // Filter out whitespace for easier testing
        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 3, "Tokens: {meaningful:?}");
        let mut meaningful_iter = meaningful.iter();
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "key")
        );
        assert!(matches!(meaningful_iter.next().unwrap().0, Token::Colon));
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "value")
        );
    }

    #[test]
    fn test_tokenize_flow_mapping() {
        let (tokens, errors) = tokenize("{a: 1}");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 5);
        let mut meaningful_iter = meaningful.iter();
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::FlowMapStart
        ));
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "a")
        );
        assert!(matches!(meaningful_iter.next().unwrap().0, Token::Colon));
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "1")
        );
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::FlowMapEnd
        ));
    }

    #[test]
    fn test_tokenize_document_start() {
        let (tokens, errors) = tokenize("---");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful.first().unwrap().0, Token::DocStart));
    }

    #[test]
    fn test_tokenize_double_quoted() {
        // Note: The chumsky lexer emits simplified StringContent tokens
        let (tokens, errors) = tokenize("\"hello\\nworld\"");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::StringContent(ref string) if string == "hello\nworld")
        );
    }

    #[test]
    fn test_tokenize_single_quoted() {
        // Note: The chumsky lexer emits simplified StringContent tokens
        let (tokens, errors) = tokenize("'hello''world'");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::StringContent(ref string) if string == "hello'world")
        );
    }

    #[test]
    fn test_tokenize_anchor_alias() {
        let (tokens, errors) = tokenize("&myanchor *myalias");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 2);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::Anchor(ref string) if string == "myanchor")
        );
        assert!(
            matches!(meaningful.last().unwrap().0, Token::Alias(ref string) if string == "myalias")
        );
    }

    #[test]
    fn test_tokenize_sequence() {
        let (tokens, errors) = tokenize("- item1\n- item2");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace))
            .collect();

        // LineStart(0), BlockSeqIndicator, Plain, LineStart(0), BlockSeqIndicator, Plain
        let mut meaningful_iter = meaningful.iter();
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::LineStart(0)
        ));
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::BlockSeqIndicator
        ));
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "item1")
        );
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::LineStart(0)
        ));
        assert!(matches!(
            meaningful_iter.next().unwrap().0,
            Token::BlockSeqIndicator
        ));
        assert!(
            matches!(meaningful_iter.next().unwrap().0, Token::Plain(ref string) if string == "item2")
        );
    }

    #[test]
    fn test_tokenize_comment() {
        let (tokens, errors) = tokenize("key: value # comment");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let comments: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| matches!(token, Token::Comment(_)))
            .collect();

        assert_eq!(comments.len(), 1);
        assert!(
            matches!(comments.first().unwrap().0, Token::Comment(ref string) if string == " comment")
        );
    }

    #[test]
    fn test_tokenize_tag() {
        let (tokens, errors) = tokenize("!!str hello");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 2);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::Tag(ref string) if string == "!!str")
        );
        assert!(
            matches!(meaningful.last().unwrap().0, Token::Plain(ref string) if string == "hello")
        );
    }

    #[test]
    fn test_tokenize_asterisk_space() {
        // Test that "* bullet" (asterisk followed by space) is tokenized as plain scalar, not alias
        let (tokens, errors) = tokenize("* bullet");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::Plain(ref string) if string == "* bullet")
        );
    }

    #[test]
    fn test_tokenize_valid_alias() {
        // Test that "*alias" (asterisk followed by anchor char) is still tokenized as alias
        let (tokens, errors) = tokenize("*myalias");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(
            matches!(meaningful.first().unwrap().0, Token::Alias(ref string) if string == "myalias")
        );
    }
}
