package parser

import (
	. "no/ast"
	. "no/core"

	"errors"
	"strings"
)

func LexWord(text string, charIndex int, lineIndex int) (Token, error) {
	isWord := func(text string, word string, charIndex int) bool {
		if len(text[charIndex:]) < len(word) {
			return false
		}
		isDel := func(c byte) bool {
			if IsAlpha(c) || IsDigit(c) {
				return false
			}
			return true
		}
		wordMatches := text[charIndex:charIndex+len(word)] == word
		isDelimited := isDel(text[charIndex+len(word)])
		return wordMatches && isDelimited
	}

	span1 := Span{Start: charIndex, End: charIndex + 1}
	span2 := Span{Start: charIndex, End: charIndex + 2}
	span3 := Span{Start: charIndex, End: charIndex + 3}
	span4 := Span{Start: charIndex, End: charIndex + 4}
	span5 := Span{Start: charIndex, End: charIndex + 5}
	span6 := Span{Start: charIndex, End: charIndex + 6}
	span8 := Span{Start: charIndex, End: charIndex + 8}

	token := Token{Kind: TOKEN_WRONG, Span: span1, Line: lineIndex}

	a := CharAt(text, charIndex+0)
	b := CharAt(text, charIndex+1)
	c := CharAt(text, charIndex+2)
	d := CharAt(text, charIndex+3)

	// Parse reserrved keywords
	switch a {
	case ' ', '\t', '\n':
		token.Kind = TOKEN_WHITESPACE
	case '\r':
		panic("Unable to tokenize Carraige Return (\\r)")

	case 'a':
		if isWord(text, "and", charIndex) {
			token.Kind = TOKEN_AND
			token.Span = span3
		} else if isWord(text, "as", charIndex) {
			token.Kind = TOKEN_AS
			token.Span = span2
		}
	case 'b':
		if isWord(text, "break", charIndex) {
			token.Kind = TOKEN_BREAK
			token.Span = span5
		}
	case 'c':
		if isWord(text, "continue", charIndex) {
			token.Kind = TOKEN_CONTINUE
			token.Span = span8
		}
	case 'd':
		if isWord(text, "defer", charIndex) {
			token.Kind = TOKEN_DEFER
			token.Span = span5
		}
	case 'e':
		if isWord(text, "export", charIndex) {
			token.Kind = TOKEN_EXPORT
			token.Span = span6
		} else if isWord(text, "else", charIndex) {
			token.Kind = TOKEN_ELSE
			token.Span = span4
		} else if isWord(text, "enum", charIndex) {
			token.Kind = TOKEN_ENUM
			token.Span = span4
		}
	case 'f':
		if isWord(text, "fn", charIndex) {
			token.Kind = TOKEN_FUNCTION
			token.Span = span2
		} else if isWord(text, "float", charIndex) {
			token.Kind = TOKEN_FLOAT
			token.Span = span5
		} else if isWord(text, "false", charIndex) {
			token.Kind = TOKEN_FALSE
			token.Span = span5
		} else if isWord(text, "for", charIndex) {
			token.Kind = TOKEN_FOR
			token.Span = span3
		}
	case 'i':
		if isWord(text, "int", charIndex) {
			token.Kind = TOKEN_INT
			token.Span = span3
		} else if isWord(text, "if", charIndex) {
			token.Kind = TOKEN_IF
			token.Span = span2
		}
	case 'l':
		if isWord(text, "let", charIndex) {
			token.Kind = TOKEN_LET
			token.Span = span3
		} else if isWord(text, "local", charIndex) {
			token.Kind = TOKEN_LOCAL
			token.Span = span5
		}
	case 'm':
		if a == 'm' && b == 'u' && c == 't' && d == '&' {
			token.Kind = TOKEN_MUTAMPERSAND
			token.Span = span4
		} else if isWord(text, "mut", charIndex) {
			token.Kind = TOKEN_MUT
			token.Span = span3
		} else if isWord(text, "match", charIndex) {
			token.Kind = TOKEN_MATCH
			token.Span = span5
		} else if isWord(text, "mod", charIndex) {
			token.Kind = TOKEN_MOD
			token.Span = span3
		}
	case 'n':
		if isWord(text, "null", charIndex) {
			token.Kind = TOKEN_NULL
			token.Span = span4
		}
	case 'o':
		if isWord(text, "or", charIndex) {
			token.Kind = TOKEN_OR
			token.Span = span2
		}
	case 'r':
		if isWord(text, "return", charIndex) {
			token.Kind = TOKEN_RETURN
			token.Span = span6
		}
	case 's':
		if isWord(text, "struct", charIndex) {
			token.Kind = TOKEN_STRUCT
			token.Span = span6
		}
	case 't':
		if isWord(text, "true", charIndex) {
			token.Kind = TOKEN_TRUE
			token.Span = span4
		} else if isWord(text, "type", charIndex) {
			token.Kind = TOKEN_TYPE
			token.Span = span4
		}
	case 'u':
		if isWord(text, "use", charIndex) {
			token.Kind = TOKEN_USE
			token.Span = span3
		}
	case 'w':
		if isWord(text, "while", charIndex) {
			token.Kind = TOKEN_WHILE
			token.Span = span5
		}
	case 'y':
		if isWord(text, "yield", charIndex) {
			token.Kind = TOKEN_YIELD
			token.Span = span5
		}

	case '!':
		token.Kind = TOKEN_EXCLAMATION
	case ';':
		token.Kind = TOKEN_SEMICOLON
	case '&':
		token.Kind = TOKEN_AMPERSAND
	case '\\':
		token.Kind = TOKEN_BACKSLASH
	case '?':
		token.Kind = TOKEN_QUESTION
	case ':':
		token.Kind = TOKEN_COLON
	case ',':
		token.Kind = TOKEN_COMMA
	case '#':
		token.Kind = TOKEN_HASH
	case '|':
		token.Kind = TOKEN_BAR

	case '+':
		token.Kind = TOKEN_PLUS
	case '-':
		if a == '-' && b == '>' {
			token.Kind = TOKEN_RARROW
			token.Span = span2
		} else {
			token.Kind = TOKEN_MINUS
		}
	case '*':
		token.Kind = TOKEN_STAR
	case '/':
		nextNewLine := func() int {
			i := strings.Index(text[charIndex:], "\n")
			if i >= 0 {
				return i
			}
			return len(text[charIndex:])
		}
		if a == '/' && b == '/' && c == '/' {
			token.Kind = TOKEN_DOCUMENT
			token.Span.End = charIndex + nextNewLine()
		} else if a == '/' && b == '/' {
			token.Kind = TOKEN_COMMENT
			token.Span.End = charIndex + nextNewLine()
		} else {
			token.Kind = TOKEN_SLASH
		}
	case '=':
		if a == '=' && b == '=' {
			token.Kind = TOKEN_EQEQ
			token.Span = span2
		} else {
			token.Kind = TOKEN_EQ
		}
	case '<':
		if a == '<' && b == '=' {
			token.Kind = TOKEN_LTEQ
			token.Span = span2
		} else {
			token.Kind = TOKEN_LT
		}
	case '>':
		if a == '>' && b == '=' {
			token.Kind = TOKEN_GTEQ
			token.Span = span2
		} else {
			token.Kind = TOKEN_GT
		}
	case '(':
		token.Kind = TOKEN_LPAREN
	case ')':
		token.Kind = TOKEN_RPAREN
	case '{':
		token.Kind = TOKEN_LBRACE
	case '}':
		token.Kind = TOKEN_RBRACE
	case '[':
		token.Kind = TOKEN_LBRACK
	case ']':
		token.Kind = TOKEN_RBRACK
	}

	// Parse identifiers and numbers
	if token.Kind == TOKEN_WRONG {
		if IsAlpha(a) || a == '_' {
			wordEnd := func() int {
				for i, c := range text[charIndex:] {
					if !IsAlpha(byte(c)) && c != '_' {
						return charIndex + i
					}
				}
				return len(text)
			}()
			token.Kind = TOKEN_XNAME
			token.Span.End = wordEnd
		} else if IsDigit(a) {
			wordEnd := func() int {
				for i, c := range text[charIndex:] {
					if !IsDigit(byte(c)) {
						return charIndex + i
					}
				}
				return len(text)
			}()
			token.Kind = TOKEN_XINT
			token.Span.End = wordEnd
		}
	}

	if token.Kind == TOKEN_WRONG {
		return token, errors.New("Unable to tokenize code.")
	} else {
		return token, nil
	}
}
