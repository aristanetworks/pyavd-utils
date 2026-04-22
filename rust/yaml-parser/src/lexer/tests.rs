// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::*;

fn plain(text: &str) -> Token<'_> {
    Token::Plain(text.into())
}

fn plain_key(text: &str) -> Token<'_> {
    Token::Plain(PlainScalarToken::new(
        text.into(),
        PlainScalarMeta {
            terminated_by_colon: true,
            terminated_by_comment: false,
            may_continue_on_next_line_in_flow: false,
        },
    ))
}

fn get_tokens(input: &str) -> Vec<Token<'_>> {
    let (tokens, _errors) = tokenize_document(input);
    tokens
        .into_iter()
        .filter(|rt| {
            !matches!(
                rt.token,
                Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_)
            )
        })
        .map(|rt| rt.token)
        .collect()
}

#[test]
fn test_colon_in_block_plain_scalar() {
    // In block context, :foo is a plain scalar
    let tokens = get_tokens(":foo");
    assert_eq!(tokens, vec![plain(":foo")]);
}

#[test]
fn test_colon_in_flow_context() {
    // In flow context, key: value is a mapping. key:value (no space) is also
    // parsed as key + : + value because : followed by a flow indicator or
    // end of content terminates the key.
    // But key:value where : is followed by non-whitespace non-flow-indicator
    // is the key as the whole "key:value" plain scalar!
    // Actually, let's check: {key:value} - the colon is followed by 'v' which
    // is not whitespace/flow-indicator, so "key:value" is ONE plain scalar.
    let tokens = get_tokens("{key:value}");
    assert_eq!(
        tokens,
        vec![Token::FlowMapStart, plain("key:value"), Token::FlowMapEnd,]
    );
}

#[test]
fn test_colon_with_space_in_flow() {
    // With a space after colon, it's a mapping
    let tokens = get_tokens("{key: value}");
    assert_eq!(
        tokens,
        vec![
            Token::FlowMapStart,
            plain_key("key"),
            Token::Colon,
            plain("value"),
            Token::FlowMapEnd,
        ]
    );
}

#[test]
fn test_colon_adjacent_in_flow() {
    // {:value} - the colon is followed by 'v', not whitespace, so :value is
    // a plain scalar starting with colon
    let tokens = get_tokens("{:value}");
    assert_eq!(
        tokens,
        vec![Token::FlowMapStart, plain(":value"), Token::FlowMapEnd,]
    );
}

#[test]
fn test_colon_as_indicator_in_flow() {
    // {: value} - colon followed by space is an indicator (empty key)
    let tokens = get_tokens("{: value}");
    assert_eq!(
        tokens,
        vec![
            Token::FlowMapStart,
            Token::Colon,
            plain("value"),
            Token::FlowMapEnd,
        ]
    );
}

#[test]
fn test_quoted_key_adjacent_value() {
    // After quoted key, :value should be : + value (not :value as scalar)
    // This is test C2DT from yaml-test-suite
    let tokens = get_tokens("{\"adjacent\":value}");
    assert_eq!(
        tokens,
        vec![
            Token::FlowMapStart,
            Token::StringStart(QuoteStyle::Double),
            Token::StringContent("adjacent".into()),
            Token::StringEnd(QuoteStyle::Double),
            Token::Colon,
            plain("value"),
            Token::FlowMapEnd,
        ]
    );
}

#[test]
fn test_comma_in_block_plain_scalar() {
    // In block context, comma is part of plain scalar
    let tokens = get_tokens("a,b,c");
    assert_eq!(tokens, vec![plain("a,b,c")]);
}

#[test]
fn test_comma_in_flow_context() {
    // In flow context, comma is separator
    let tokens = get_tokens("[a,b,c]");
    assert_eq!(
        tokens,
        vec![
            Token::FlowSeqStart,
            plain("a"),
            Token::Comma,
            plain("b"),
            Token::Comma,
            plain("c"),
            Token::FlowSeqEnd,
        ]
    );
}

#[test]
fn test_brackets_in_block_plain_scalar() {
    // In block context, [] are part of plain scalar...
    // Actually, no - [ and ] start/end flow sequences even in block context.
    // The difference is that , is only a separator in flow context.
    let tokens = get_tokens("key: [value]");
    // This should still recognize [] as flow sequence
    assert!(tokens.contains(&Token::FlowSeqStart));
}

#[test]
fn test_percent_in_document() {
    // % in document content is just a regular character
    let tokens = get_tokens("foo%bar");
    assert_eq!(tokens, vec![plain("foo%bar")]);
}

#[test]
fn test_percent_at_line_start_in_flow() {
    // % at line start inside flow mapping should be plain scalar content
    let tokens = get_tokens("{ matches\n% : 20 }");
    // Should have: { matches <newline> % : 20 }
    // The % is part of multiline plain scalar, then : 20 is value
    assert!(tokens.contains(&Token::FlowMapStart));
    assert!(tokens.contains(&Token::FlowMapEnd));
    // Should NOT have a reserved directive
    assert!(
        !tokens
            .iter()
            .any(|token| matches!(token, Token::ReservedDirective(_)))
    );
}

#[test]
fn test_colon_value_in_flow() {
    // {x: :x} - value is :x (plain scalar starting with colon)
    let tokens = get_tokens("{x: :x}");
    // Should be: { x : :x }
    assert_eq!(
        tokens,
        vec![
            Token::FlowMapStart,
            plain_key("x"),
            Token::Colon,
            plain(":x"),
            Token::FlowMapEnd,
        ]
    );
}

#[test]
fn test_url_in_flow() {
    // URL in flow context
    let tokens = get_tokens("{url: http://example.org}");
    // Should recognize http://example.org as a plain scalar
    assert!(tokens.contains(&plain("http://example.org")));
}

// Tests for tokenize_document
//
// Note: Whitespace, WhitespaceWithTabs, and Comment are real tokens in the stream
// because comments have semantic meaning in YAML (they terminate plain scalars).

#[test]
fn test_simple_mapping_tokens() {
    // Test token stream for simple mapping
    let (tokens, errors) = tokenize_document("key: value");
    assert!(errors.is_empty());

    // Should have 5 tokens: LineStart(0), "key", ":", Whitespace, "value"
    assert_eq!(tokens.len(), 5);

    // Check token types
    assert!(matches!(tokens[0].token, Token::LineStart(0)));
    assert_eq!(tokens[1].token, plain_key("key"));
    assert_eq!(tokens[2].token, Token::Colon);
    assert_eq!(tokens[3].token, Token::Whitespace);
    assert_eq!(tokens[4].token, plain("value"));
    assert!(matches!(
        &tokens[1].token,
        Token::Plain(plain) if plain.meta().terminated_by_colon
    ));
}

#[test]
fn test_comment_as_token() {
    // Test that comments are real tokens
    let (tokens, errors) = tokenize_document("key: value # this is a comment");
    assert!(errors.is_empty());

    assert!(matches!(
        tokens
            .iter()
            .find(|token| matches!(&token.token, Token::Plain(plain) if plain.as_str() == "value")),
        Some(token) if matches!(
            &token.token,
            Token::Plain(plain) if plain.meta().terminated_by_comment
        )
    ));

    // Comment should be a real token in the stream
    let comment_token = tokens
        .iter()
        .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("this is a comment")));
    assert!(comment_token.is_some(), "Should find comment token");
}

#[test]
fn test_multiline_with_comments() {
    // Test multiline document with comments
    let input = "# header comment\nkey: value";
    let (tokens, errors) = tokenize_document(input);
    assert!(errors.is_empty());

    // First token is LineStart(0)
    assert!(matches!(tokens[0].token, Token::LineStart(0)));

    // Comment should be a real token
    let comment_token = tokens
        .iter()
        .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("header")));
    assert!(comment_token.is_some(), "Should find header comment token");
}

#[test]
fn test_empty_input() {
    let (tokens, errors) = tokenize_document("");
    assert!(errors.is_empty());
    // Empty input still gets initial LineStart(0) token
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0].token, Token::LineStart(0)));
}

#[test]
fn test_comment_only() {
    // Only comments
    let (tokens, errors) = tokenize_document("# just a comment");
    assert!(errors.is_empty());
    // LineStart(0) and Comment are separate tokens
    assert_eq!(tokens.len(), 2);
    assert!(matches!(tokens[0].token, Token::LineStart(0)));
    assert!(matches!(&tokens[1].token, Token::Comment(s) if s.contains("just a comment")));
}

#[test]
fn test_tab_as_whitespace_with_tabs_token() {
    // Test that tabs are WhitespaceWithTabs tokens (for error detection)
    let input = "key:\n\tvalue";
    let (tokens, errors) = tokenize_document(input);
    assert!(errors.is_empty());

    // WhitespaceWithTabs should be a real token
    let tab_token = tokens
        .iter()
        .find(|t| matches!(t.token, Token::WhitespaceWithTabs));
    assert!(
        tab_token.is_some(),
        "Should find WhitespaceWithTabs token for tab"
    );
}

#[test]
fn test_multi_document_with_directives() {
    // Test that Lexer correctly handles multi-document streams
    // with directives, document markers, and phase transitions
    let input = "%YAML 1.2\n---\ndoc1\n...\n%TAG !e! tag:example,2000:\n---\ndoc2\n";
    let tokens = get_tokens(input);

    // Should have: YamlDirective, DocStart, Plain(doc1), DocEnd,
    //              TagDirective, DocStart, Plain(doc2)
    assert!(
        tokens.iter().any(|t| matches!(t, Token::YamlDirective(_))),
        "Should have YAML directive"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token, Token::TagDirective(..))),
        "Should have TAG directive after ..."
    );
    assert_eq!(
        tokens
            .iter()
            .filter(|t| matches!(t, Token::DocStart))
            .count(),
        2,
        "Should have two DocStart markers"
    );
    assert_eq!(
        tokens.iter().filter(|t| matches!(t, Token::DocEnd)).count(),
        1,
        "Should have one DocEnd marker"
    );
}

#[test]
fn test_multi_document_with_directives_crlf() {
    let input = "%YAML 1.2\r\n---\r\ndoc1\r\n...\r\n%TAG !e! tag:example,2000:\r\n---\r\ndoc2\r\n";
    let tokens = get_tokens(input);

    assert!(
        tokens.iter().any(|t| matches!(t, Token::YamlDirective(_))),
        "Should have YAML directive"
    );
    assert!(
        tokens
            .iter()
            .any(|token| matches!(token, Token::TagDirective(..))),
        "Should have TAG directive after ..."
    );
    assert_eq!(
        tokens
            .iter()
            .filter(|t| matches!(t, Token::DocStart))
            .count(),
        2,
        "Should have two DocStart markers"
    );
    assert_eq!(
        tokens.iter().filter(|t| matches!(t, Token::DocEnd)).count(),
        1,
        "Should have one DocEnd marker"
    );
}

#[test]
fn test_non_ascii_line_separators_are_not_newlines() {
    let tokens = get_tokens("foo\u{0085}bar\u{2028}baz\u{2029}qux");
    assert_eq!(tokens, vec![plain("foo\u{0085}bar\u{2028}baz\u{2029}qux")]);
}
