module lox_scanner;

import core.stdc.stdlib;
import std.conv;
import std.stdio;

struct Scanner {
    int start;
    int current;
    int line;
    char[] source;
}

enum TokenType {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN,
  LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS,
  SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE,
  FOR, FUN, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS,
  TRUE, VAR, WHILE,

  ERROR,
  EOF
}

struct Token {
    TokenType type;
    int line;
    char[] content;
}

// ughhhhhh i fucking hate these C-style globals
Scanner scanner;

void initScanner(string source) {
    scanner.source = source.dup;
    scanner.start = 0;
    scanner.current = 0;
    scanner.line = 1;
}

bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
}

Token *scanToken() {
    skipWhitespace();

    scanner.start = scanner.current;

    if (atEnd()) {
        return new Token(TokenType.EOF);
    }

    auto c = advance();

    if (isAlpha(c)) {
        return scanIdentifier();
    }

    if (isDigit(c)) {
        return scanNumber();
    }

    switch (c) {
        case '(': return makeToken(TokenType.LEFT_PAREN);
        case ')': return makeToken(TokenType.RIGHT_PAREN);
        case '{': return makeToken(TokenType.LEFT_BRACE);
        case '}': return makeToken(TokenType.RIGHT_BRACE);
        case ';': return makeToken(TokenType.SEMICOLON);
        case ',': return makeToken(TokenType.COMMA);
        case '.': return makeToken(TokenType.DOT);
        case '-': return makeToken(TokenType.MINUS);
        case '+': return makeToken(TokenType.PLUS);
        case '/': return makeToken(TokenType.SLASH);
        case '*': return makeToken(TokenType.STAR);
        case '!':
            return makeToken(
                match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
        case '=':
            return makeToken(
                match('=') ? TokenType.EQUAL_EQUAL : TokenType.EQUAL);
        case '<':
            return makeToken(
                match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
        case '>':
            return makeToken(
                match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);        
        case '"':
            return scanString();
        default:
            break;
    }

    return errorToken("Unexpected character.");
}

void skipWhitespace() {
    while (true) {
        auto c = peek();
        switch (c) {
            case '\n':
                scanner.line++;
            case ' ':            
            case '\r':
            case '\t':
                advance();
                break;

            case '/':
                if (peekNext() == '/') {
                    while (peek() != '\n' && !atEnd()) {
                        advance();
                    }
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

unittest {
    initScanner("for foo");
    const auto forToken = scanToken();
    const auto fooToken = scanToken();

    assert(forToken.type == TokenType.FOR);
    assert(forToken.content == "for");
    assert(forToken.line == 1);
    assert(fooToken.type == TokenType.IDENTIFIER);
    assert(fooToken.content == "foo");
    assert(fooToken.line == 1);

    initScanner("{ else if }");
    const auto openToken = scanToken();
    const auto elseToken = scanToken();
    const auto ifToken = scanToken();
    const auto closeToken = scanToken();

    assert(openToken.type  == TokenType.LEFT_BRACE);
    assert(elseToken.type  == TokenType.ELSE);
    assert(ifToken.type    == TokenType.IF);
    assert(closeToken.type == TokenType.RIGHT_BRACE);
}

TokenType checkKeyword(int start, int length, string rest, TokenType type) {

    if (scanner.current - scanner.start == start + length &&
        samesies(to!string(scanner.source[(scanner.start + start)..$]), rest, length)) {
        return type;
    }

    return TokenType.IDENTIFIER;
}

// cool function, 100% good boy
bool samesies(string str1, string str2, size_t length) {
    // yes this probably exists but i don't care

    auto i = 0;

    while (i < length) {
        if (i >= str1.length || i >= str2.length) {
            return false;
        }

        if (str1[i] != str2[i]) {
            return false;
        }

        i++;
    } 

    return true;
}

unittest {
    assert(samesies("test", "test123", 4));
    assert(samesies("test1234", "test", 4));
    assert(!samesies("", "test", 4));
    assert(!samesies("test", "", 4));
 }


TokenType identifierType() {
    switch (scanner.source[scanner.start]) {
        case 'a': return checkKeyword(1, 2, "nd", TokenType.AND);
        case 'c': return checkKeyword(1, 4, "lass", TokenType.CLASS);
        case 'e': return checkKeyword(1, 3, "lse", TokenType.ELSE);
        case 'f': 
            switch(scanner.source[scanner.start+1]) {
                case 'a': return checkKeyword(2, 3, "lse", TokenType.FALSE);
                case 'o': return checkKeyword(2, 1, "r", TokenType.FOR);
                case 'u': return checkKeyword(2, 2, "un", TokenType.FUN);
                default: return TokenType.IDENTIFIER;
            }
        case 'i': return checkKeyword(1, 1, "f", TokenType.IF);
        case 'n': return checkKeyword(1, 2, "il", TokenType.NIL);
        case 'o': return checkKeyword(1, 1, "r", TokenType.OR);
        case 'p': return checkKeyword(1, 4, "rint", TokenType.PRINT);
        case 'r': return checkKeyword(1, 5, "eturn", TokenType.RETURN);
        case 's': return checkKeyword(1, 4, "uper", TokenType.SUPER);
        case 't':
            switch(scanner.source[scanner.start+1]) {
                case 'h': return checkKeyword(1, 2, "is", TokenType.THIS);
                case 'r': return checkKeyword(1, 3, "rue", TokenType.TRUE);
                default: return TokenType.IDENTIFIER;
            }
        case 'v': return checkKeyword(1, 2, "ar", TokenType.VAR);
        case 'w': return checkKeyword(1, 4, "hile", TokenType.WHILE);
        default:
            return TokenType.IDENTIFIER;
    }
}

Token * scanIdentifier() {
    while (isAlpha(peek()) || isDigit(peek())) {
        advance();
    }

    return makeToken(identifierType());
}

Token *scanNumber() {
    while (isDigit(peek())) {
        advance();
    }

    if (peek() == '.' && isDigit(peekNext())) {
        advance();
        while(isDigit(peek())) {
            advance();
        }
    }

    return makeToken(TokenType.NUMBER);
}

Token *scanString() {
    while (peek() != '"' && !atEnd()) {
        if (peek() == '\n') {
            scanner.line++;
        }
        advance();
    }

    if (atEnd()) {
        return errorToken("Unterminated string.");
    }

    advance();

    return makeToken(TokenType.STRING);
}

char peek() { 

    if (scanner.current >= scanner.source.length) {
        return '\0';
    }

    return scanner.source[scanner.current];
}

char peekNext() {
    if (atEnd) {
        return '\0';
    }

    return scanner.source[scanner.current + 1];
}

char advance() {
    scanner.current++;
    return scanner.source[scanner.current - 1];
}

bool match(char expected) {
    if (atEnd) return false;
    if (scanner.source[scanner.current] != expected) return false;

    scanner.current++;
    return true;
}

bool atEnd() { 
    return (scanner.source.length <= scanner.current) || (scanner.source[scanner.current] == '\0');
}

Token *makeToken(TokenType token) {
    auto content = scanner.source[scanner.start..scanner.current];
    return new Token(token, scanner.line, content);
} 

Token *errorToken(string message) {
    return new Token(TokenType.ERROR, scanner.line, message.dup);
}