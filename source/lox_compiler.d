module lox_compiler;

import std.stdio;
import std.conv;

import lox_debug;
import lox_scanner;
import common;
import value;
import std.typecons;

struct Parser {
    Token *current;
    Token *previous;
    bool hadError;
    bool panicMode;
}

enum Precedence {
    NONE,
    ASSIGNMENT,  // =
    OR,          // or
    AND,         // and
    EQUALITY,    // == !=
    COMPARISON,  // < > <= >=
    TERM,        // + -
    FACTOR,      // * /
    UNARY,       // ! -
    CALL,        // . ()
    PRIMARY
}

alias ParseFunc = Typedef!(void function());

struct ParseRule { 
    void function() prefix;
    void function() infix;
    Precedence precedence;
}

Parser parser;

Chunk* compilingChunk;

Chunk* currentChunk() { 
    return compilingChunk;
}

void error(string message) {
    errorAt(parser.previous, message);
}

void errorAt(Token *token, string message) {
    if (parser.panicMode) {
        return;
    }

    parser.panicMode = true;
    stderr.writef("[line %d] Error", token.line);

    if (token.type == TokenType.EOF) {
        stderr.writef(" at end");
    } else if (token.type == TokenType.ERROR) {
        // ?
    } else {
        // the C for this was:
        // fprintf(stderr, " at '%.*s'", token->length, token->start);
        // which to me is funky - '%.*s' must be something special
        stderr.writef(" at '%s'", token.content);
    }

    stderr.writef(": %s\n", message);
    parser.hadError = true;
}

void errorAtCurrent(string message) {
    errorAt(parser.previous, message);
}

void advance(){
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken();

        if (parser.current.type != TokenType.ERROR) {
            break;
        }

        errorAtCurrent(to!string(parser.current.content));
    }
}

void consume(TokenType type, string message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

void emitByte(ubyte b) {
    currentChunk().write(b, parser.previous.line);
}

void emitBytes(ubyte b1, ubyte b2) {
    emitByte(b1);
    emitByte(b2);
}

void emitReturn() { 
    emitByte(OpCode.RETURN);
}

ubyte makeConstant(Value value) {
    const auto constant = currentChunk().addConstant(value);

    if (constant > 255) {
        error("Too many constants within one chunk.");
        return 0;
    }

    return constant;
}

void emitConstant(Value value) {
    emitBytes(OpCode.CONSTANT, makeConstant(value));
}

void endCompiler() { 
    emitReturn();

    debug {
        if (!parser.hadError) {
            disassembleChunk(currentChunk(), "code");
        }
    }
}

void binary() {
    const TokenType operatorType = parser.previous.type;

    ParseRule rule = getRule(operatorType);
    parsePrecedence(to!Precedence(rule.precedence + 1));

    switch (operatorType) {
        case TokenType.PLUS:  emitByte(OpCode.ADD); break;
        case TokenType.MINUS: emitByte(OpCode.SUBTRACT); break;
        case TokenType.STAR:  emitByte(OpCode.MULTIPLY); break;
        case TokenType.SLASH: emitByte(OpCode.DIVIDE); break;
        default:
            return;
    }
}

void grouping() {
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
}

void number() { 
    auto value = to!double(parser.previous.content);
    emitConstant(Value(value));
}

void unary() {
    const TokenType operatorType = parser.previous.type;

    parsePrecedence(Precedence.UNARY);

    switch (operatorType) { 
        case TokenType.MINUS:
            emitByte(OpCode.NEGATE);
            break;

        default:
            return;
    }
}

ParseRule[TokenType] rules = null;

void initRules() {
    if (rules != null) return;

    // kinda annoying that I'm doing this at runtime :-/
    rules = [
        TokenType.RIGHT_PAREN   : ParseRule(null,    null,    Precedence.NONE),
        TokenType.LEFT_BRACE    : ParseRule(null,    null,    Precedence.NONE), 
        TokenType.RIGHT_BRACE   : ParseRule(null,    null,    Precedence.NONE),
        TokenType.COMMA         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.DOT           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.MINUS         : ParseRule(&unary,  &binary, Precedence.TERM),
        TokenType.PLUS          : ParseRule(null,    &binary, Precedence.TERM),
        TokenType.SEMICOLON     : ParseRule(null,    null,    Precedence.NONE),
        TokenType.SLASH         : ParseRule(null,    &binary, Precedence.FACTOR),
        TokenType.STAR          : ParseRule(null,    &binary, Precedence.FACTOR),
        TokenType.BANG          : ParseRule(null,    null,    Precedence.NONE),
        TokenType.BANG_EQUAL    : ParseRule(null,    null,    Precedence.NONE),
        TokenType.EQUAL         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.EQUAL_EQUAL   : ParseRule(null,    null,    Precedence.NONE),
        TokenType.GREATER       : ParseRule(null,    null,    Precedence.NONE),
        TokenType.GREATER_EQUAL : ParseRule(null,    null,    Precedence.NONE),
        TokenType.LESS          : ParseRule(null,    null,    Precedence.NONE),
        TokenType.LESS_EQUAL    : ParseRule(null,    null,    Precedence.NONE),
        TokenType.IDENTIFIER    : ParseRule(null,    null,    Precedence.NONE),
        TokenType.STRING        : ParseRule(null,    null,    Precedence.NONE),
        TokenType.NUMBER        : ParseRule(&number, null,    Precedence.NONE),
        TokenType.AND           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.CLASS         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.ELSE          : ParseRule(null,    null,    Precedence.NONE),
        TokenType.FALSE         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.FOR           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.FUN           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.IF            : ParseRule(null,    null,    Precedence.NONE),
        TokenType.NIL           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.OR            : ParseRule(null,    null,    Precedence.NONE),
        TokenType.PRINT         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.RETURN        : ParseRule(null,    null,    Precedence.NONE),
        TokenType.SUPER         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.THIS          : ParseRule(null,    null,    Precedence.NONE),
        TokenType.TRUE          : ParseRule(null,    null,    Precedence.NONE),
        TokenType.VAR           : ParseRule(null,    null,    Precedence.NONE),
        TokenType.WHILE         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.ERROR         : ParseRule(null,    null,    Precedence.NONE),
        TokenType.EOF           : ParseRule(null,    null,    Precedence.NONE)
    ];
}

void parsePrecedence(Precedence precedence) {
    advance();
    auto prefixRule = getRule(parser.previous.type).prefix;

    if (prefixRule == null) {
        error("Expected expression.");
        return;
    }

    prefixRule();

    while (precedence <= getRule(parser.current.type).precedence) {
        advance();
        auto infixRule = getRule(parser.previous.type).infix;
        infixRule();
    }
}

ParseRule getRule(TokenType type) {
    return rules[type];
}

void expression() {
    parsePrecedence(Precedence.ASSIGNMENT);
}

bool compile(string source, Chunk *chunk) {
    initRules();
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    expression();
    consume(TokenType.EOF, "Expected end of expression");
    endCompiler();
    return !parser.hadError;

    // int line = -1;
    // while (true) {
    //     auto token = scanToken();

    //     if (token.line != line) {
    //         writef("%4d ", token.line);
    //         line = token.line;
    //     } else {
    //         writef("    | ");
    //     }

    //     writefln("%2d '%s'", token.type, to!string(token.content));

    //     if (token.type == TokenType.EOF) break;
    // }
}