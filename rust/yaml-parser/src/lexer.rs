// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML lexer (tokenizer) with error recovery.
//!
//! This module implements the first phase of parsing: converting the
//! character stream into a token stream. The lexer uses chumsky for
//! error recovery, allowing it to continue tokenizing after errors.

use chumsky::prelude::*;

use crate::span::{Span, Spanned};

/// Quote style for quoted strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuoteStyle {
    /// Single quote (')
    Single,
    /// Double quote (")
    Double,
}

/// A YAML token.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Indicators (single characters with special meaning)
    /// `-` block sequence entry indicator (when followed by whitespace or newline)
    BlockSeqIndicator,
    /// `?` mapping key indicator (when followed by whitespace or newline)
    MappingKey,
    /// `:` mapping value indicator
    Colon,
    /// `{` flow mapping start
    FlowMapStart,
    /// `}` flow mapping end
    FlowMapEnd,
    /// `[` flow sequence start
    FlowSeqStart,
    /// `]` flow sequence end
    FlowSeqEnd,
    /// `,` flow entry separator
    Comma,

    // Document markers
    /// `---` document start
    DocStart,
    /// `...` document end
    DocEnd,

    // Scalars
    /// A plain (unquoted) scalar
    Plain(String),
    /// Opening quote for a quoted string (' or ")
    StringStart(QuoteStyle),
    /// Closing quote for a quoted string (' or ")
    StringEnd(QuoteStyle),
    /// Content segment inside a quoted string (escapes already processed)
    StringContent(String),
    /// A literal block scalar (`|`) - header info only, content parsed separately
    LiteralBlockHeader(BlockScalarHeader),
    /// A folded block scalar (`>`) - header info only, content parsed separately
    FoldedBlockHeader(BlockScalarHeader),

    // Anchors and aliases
    /// Anchor definition (`&name`)
    Anchor(String),
    /// Alias reference (`*name`)
    Alias(String),

    // Tags
    /// Tag (`!tag` or `!!type` or `!<uri>`)
    Tag(String),

    // Directives
    /// `%YAML` directive
    YamlDirective(String),
    /// `%TAG` directive
    TagDirective(String),
    /// Reserved/unknown directive (e.g., `%FOO`)
    ReservedDirective(String),

    // Whitespace and structure
    /// Start of a new line with indentation (number of spaces)
    LineStart(usize),
    /// Whitespace (spaces only, not at line start)
    Whitespace,
    /// Comment content (after `#`, without the `#` prefix)
    Comment(String),

    // Indentation structure (Python-style INDENT/DEDENT)
    /// Indentation increased to this level (emitted after LineStart when indent > stack.top)
    Indent(usize),
    /// Indentation decreased by one level (emitted when indent < stack.top, one per level popped)
    Dedent,

    /// Invalid token (for error recovery)
    Invalid,
}

/// Block scalar header information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockScalarHeader {
    /// Explicit indentation indicator (1-9), or None for auto-detect.
    pub indent: Option<u8>,
    /// Chomping behavior for trailing newlines.
    pub chomping: Chomping,
}

/// Block scalar chomping indicator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Chomping {
    /// `-` strip all trailing newlines
    Strip,
    /// (default) clip to single trailing newline
    #[default]
    Clip,
    /// `+` keep all trailing newlines
    Keep,
}

impl Token {
    /// Returns `true` if this is a scalar token.
    /// Note: StringStart/StringEnd/StringContent are components of a quoted scalar,
    /// not complete scalars themselves.
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        matches!(self, Self::Plain(_))
    }

    /// Returns `true` if this is a flow indicator.
    #[must_use]
    pub const fn is_flow_indicator(&self) -> bool {
        matches!(
            self,
            Self::FlowMapStart
                | Self::FlowMapEnd
                | Self::FlowSeqStart
                | Self::FlowSeqEnd
                | Self::Comma
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BlockSeqIndicator => write!(f, "'-'"),
            Self::MappingKey => write!(f, "'?'"),
            Self::Colon => write!(f, "':'"),
            Self::FlowMapStart => write!(f, "'{{'"),
            Self::FlowMapEnd => write!(f, "'}}'"),
            Self::FlowSeqStart => write!(f, "'['"),
            Self::FlowSeqEnd => write!(f, "']'"),
            Self::Comma => write!(f, "','"),
            Self::DocStart => write!(f, "'---'"),
            Self::DocEnd => write!(f, "'...'"),
            Self::Plain(s) => write!(f, "plain scalar '{s}'"),
            Self::StringStart(QuoteStyle::Single) => write!(f, "string start (')"),
            Self::StringStart(QuoteStyle::Double) => write!(f, "string start (\")"),
            Self::StringEnd(QuoteStyle::Single) => write!(f, "string end (')"),
            Self::StringEnd(QuoteStyle::Double) => write!(f, "string end (\")"),
            Self::StringContent(s) => write!(f, "string content '{s}'"),
            Self::LiteralBlockHeader(_) => write!(f, "'|'"),
            Self::FoldedBlockHeader(_) => write!(f, "'>'"),
            Self::Anchor(name) => write!(f, "anchor '&{name}'"),
            Self::Alias(name) => write!(f, "alias '*{name}'"),
            Self::Tag(tag) => write!(f, "tag '{tag}'"),
            Self::YamlDirective(v) => write!(f, "%YAML {v}"),
            Self::TagDirective(v) => write!(f, "%TAG {v}"),
            Self::ReservedDirective(v) => write!(f, "%{v}"),
            Self::LineStart(n) => write!(f, "line start (indent={n})"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Comment(c) => write!(f, "comment '{c}'"),
            Self::Indent(n) => write!(f, "INDENT({n})"),
            Self::Dedent => write!(f, "DEDENT"),
            Self::Invalid => write!(f, "<invalid>"),
        }
    }
}

/// Create the YAML lexer parser.
///
/// This returns a chumsky parser that tokenizes YAML input with error recovery.
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
        .map(|s: &str| Token::Comment(s.to_string()));

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
    let anchor_char = any().filter(|c: &char| c.is_alphanumeric() || *c == '-' || *c == '_');
    let anchor_name = anchor_char.repeated().at_least(1).to_slice();

    let anchor = just('&')
        .ignore_then(anchor_name)
        .map(|s: &str| Token::Anchor(s.to_string()));

    let alias = just('*')
        .ignore_then(anchor_name)
        .map(|s: &str| Token::Alias(s.to_string()));

    // === Tags ===
    // Verbatim tag: !<uri>
    // Shorthand tag: !handle!suffix or !suffix
    // Non-specific tag: ! or !!
    let tag_char = any().filter(|c: &char| {
        c.is_alphanumeric() || matches!(c, '-' | '_' | '.' | '/' | ':' | '#' | '%')
    });

    let tag = just('!')
        .ignore_then(
            // Verbatim tag: !<...>
            just('<')
                .ignore_then(none_of(">").repeated().to_slice())
                .then_ignore(just('>'))
                .map(|s: &str| format!("!<{s}>"))
                // Named tag handle: !!type or !handle!type or !type
                .or(just('!')
                    .ignore_then(tag_char.repeated().to_slice())
                    .map(|s: &str| format!("!!{s}")))
                // Primary tag: ! alone or !suffix
                .or(tag_char.repeated().to_slice().map(|s: &str| {
                    if s.is_empty() {
                        "!".to_string()
                    } else {
                        format!("!{s}")
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
            .validate(|s: &str, e, emitter| {
                u32::from_str_radix(s, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(e.span(), "invalid \\x escape"));
                        '\u{FFFD}'
                    })
            }),
        just('u')
            .ignore_then(text::digits(16).exactly(4).to_slice())
            .validate(|s: &str, e, emitter| {
                u32::from_str_radix(s, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(e.span(), "invalid \\u escape"));
                        '\u{FFFD}'
                    })
            }),
        just('U')
            .ignore_then(text::digits(16).exactly(8).to_slice())
            .validate(|s: &str, e, emitter| {
                u32::from_str_radix(s, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        emitter.emit(Rich::custom(e.span(), "invalid \\U escape"));
                        '\u{FFFD}'
                    })
            }),
        // Line continuation: backslash at end of line
        newline.clone().then(space.repeated()).to(' '), // Escaped newline becomes space
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
    let chomping = choice((
        just('-').to(Chomping::Strip),
        just('+').to(Chomping::Keep),
        empty().to(Chomping::Clip),
    ));

    let indent_indicator = one_of("123456789")
        .map(|c: char| Some(c.to_digit(10).unwrap() as u8))
        .or(empty().to(None));

    let block_header = indent_indicator
        .then(chomping.clone())
        .or(chomping.map(|c| (None, c)));

    let literal_header = just('|')
        .ignore_then(block_header.clone())
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
        .ignore_then(directive_content.clone())
        .map(|s: &str| Token::YamlDirective(s.trim().to_string()));
    let tag_directive = just("%TAG")
        .ignore_then(directive_content.clone())
        .map(|s: &str| Token::TagDirective(s.trim().to_string()));
    // Reserved/unknown directives: %NAME where NAME is not YAML or TAG
    let reserved_directive = just('%')
        .ignore_then(directive_content)
        .map(|s: &str| Token::ReservedDirective(s.trim().to_string()));

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
    let plain_regular_first = any().filter(move |c: &char| {
        !indicator_chars.contains(*c) && !"-?:*&".contains(*c) && !c.is_whitespace()
    });

    // -/:/? can start a plain scalar when followed by non-whitespace
    let plain_indicator_first = one_of("-?:")
        .then(none_of(" \t\n\r,[]{}").rewind())
        .map(|(c, _)| c);

    // * can start a plain scalar when NOT followed by a valid anchor char
    // (i.e., when it's not a valid alias)
    let anchor_char_set = |c: char| c.is_alphanumeric() || c == '-' || c == '_';
    let plain_asterisk = just('*')
        .then(any().filter(move |c: &char| !anchor_char_set(*c)).rewind())
        .map(|(c, _)| c);

    // & can start a plain scalar when NOT followed by a valid anchor char
    let plain_ampersand = just('&')
        .then(any().filter(move |c: &char| !anchor_char_set(*c)).rewind())
        .map(|(c, _)| c);

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
            let mut s = String::with_capacity(1 + rest.len());
            s.push(first);
            s.push_str(&rest);
            Token::Plain(s.trim_end().to_string())
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
        .map_with(|tok, e| (tok, e.span()));

    // Main token loop with error recovery
    let tokens = token
        .map_with(|tok, e| (tok, e.span()))
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
        .map(|e| {
            let span = e.span();
            ParseError {
                kind: ErrorKind::UnexpectedToken,
                span: Span::new((), span.start..span.end),
                expected: e.expected().map(|exp| format!("{exp:?}")).collect(),
                found: e.found().map(|c| c.to_string()),
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
        assert!(matches!(tokens[0].0, Token::LineStart(0)));
    }

    #[test]
    fn test_tokenize_simple_mapping() {
        let (tokens, errors) = tokenize("key: value");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        // Filter out whitespace for easier testing
        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 3, "Tokens: {meaningful:?}");
        assert!(matches!(meaningful[0].0, Token::Plain(ref s) if s == "key"));
        assert!(matches!(meaningful[1].0, Token::Colon));
        assert!(matches!(meaningful[2].0, Token::Plain(ref s) if s == "value"));
    }

    #[test]
    fn test_tokenize_flow_mapping() {
        let (tokens, errors) = tokenize("{a: 1}");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert!(matches!(meaningful[0].0, Token::FlowMapStart));
        assert!(matches!(meaningful[1].0, Token::Plain(ref s) if s == "a"));
        assert!(matches!(meaningful[2].0, Token::Colon));
        assert!(matches!(meaningful[3].0, Token::Plain(ref s) if s == "1"));
        assert!(matches!(meaningful[4].0, Token::FlowMapEnd));
    }

    #[test]
    fn test_tokenize_document_start() {
        let (tokens, errors) = tokenize("---");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful[0].0, Token::DocStart));
    }

    #[test]
    fn test_tokenize_double_quoted() {
        // Note: The chumsky lexer emits simplified StringContent tokens
        let (tokens, errors) = tokenize("\"hello\\nworld\"");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful[0].0, Token::StringContent(ref s) if s == "hello\nworld"));
    }

    #[test]
    fn test_tokenize_single_quoted() {
        // Note: The chumsky lexer emits simplified StringContent tokens
        let (tokens, errors) = tokenize("'hello''world'");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful[0].0, Token::StringContent(ref s) if s == "hello'world"));
    }

    #[test]
    fn test_tokenize_anchor_alias() {
        let (tokens, errors) = tokenize("&myanchor *myalias");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 2);
        assert!(matches!(meaningful[0].0, Token::Anchor(ref s) if s == "myanchor"));
        assert!(matches!(meaningful[1].0, Token::Alias(ref s) if s == "myalias"));
    }

    #[test]
    fn test_tokenize_sequence() {
        let (tokens, errors) = tokenize("- item1\n- item2");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace))
            .collect();

        // LineStart(0), BlockSeqIndicator, Plain, LineStart(0), BlockSeqIndicator, Plain
        assert!(matches!(meaningful[0].0, Token::LineStart(0)));
        assert!(matches!(meaningful[1].0, Token::BlockSeqIndicator));
        assert!(matches!(meaningful[2].0, Token::Plain(ref s) if s == "item1"));
        assert!(matches!(meaningful[3].0, Token::LineStart(0)));
        assert!(matches!(meaningful[4].0, Token::BlockSeqIndicator));
        assert!(matches!(meaningful[5].0, Token::Plain(ref s) if s == "item2"));
    }

    #[test]
    fn test_tokenize_comment() {
        let (tokens, errors) = tokenize("key: value # comment");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let comments: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| matches!(t, Token::Comment(_)))
            .collect();

        assert_eq!(comments.len(), 1);
        assert!(matches!(comments[0].0, Token::Comment(ref s) if s == " comment"));
    }

    #[test]
    fn test_tokenize_tag() {
        let (tokens, errors) = tokenize("!!str hello");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 2);
        assert!(matches!(meaningful[0].0, Token::Tag(ref s) if s == "!!str"));
        assert!(matches!(meaningful[1].0, Token::Plain(ref s) if s == "hello"));
    }

    #[test]
    fn test_tokenize_asterisk_space() {
        // Test that "* bullet" (asterisk followed by space) is tokenized as plain scalar, not alias
        let (tokens, errors) = tokenize("* bullet");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful[0].0, Token::Plain(ref s) if s == "* bullet"));
    }

    #[test]
    fn test_tokenize_valid_alias() {
        // Test that "*alias" (asterisk followed by anchor char) is still tokenized as alias
        let (tokens, errors) = tokenize("*myalias");
        assert!(errors.is_empty(), "Errors: {errors:?}");

        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();

        assert_eq!(meaningful.len(), 1);
        assert!(matches!(meaningful[0].0, Token::Alias(ref s) if s == "myalias"));
    }

    #[test]
    fn test_tokenize_colon_adjacent() {
        // Test tokenization of ":bar" (colon immediately followed by text)
        let (tokens, errors) = tokenize(":bar");
        println!("Tokens for ':bar': {:?}", tokens);
        println!("Errors: {:?}", errors);

        // Test tokenization of ": bar" (colon followed by space then text)
        let (tokens2, errors2) = tokenize(": bar");
        println!("Tokens for ': bar': {:?}", tokens2);
        println!("Errors: {:?}", errors2);
    }

    #[test]
    fn test_tokenize_bracket_in_plain() {
        // Test tokenization of "bla]keks" - bracket in plain scalar
        let (tokens, errors) = tokenize("bla]keks");
        println!("Tokens for 'bla]keks': {:?}", tokens);
        println!("Errors: {:?}", errors);

        // Filter out whitespace and linestart tokens
        let meaningful: Vec<_> = tokens
            .iter()
            .filter(|(t, _)| !matches!(t, Token::Whitespace | Token::LineStart(_)))
            .collect();
        println!("Meaningful tokens: {:?}", meaningful);
    }

    #[test]
    fn test_tokenize_multiline_plain() {
        // Test tokenization of "plain\n text" - multiline plain scalar
        let (tokens, errors) = tokenize("plain\n text");
        println!("Tokens for 'plain\\n text': {:?}", tokens);
        println!("Errors: {:?}", errors);
    }
}
