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

alias ParseFunc = void function(bool canAssign);

struct ParseRule { 
    ParseFunc prefix;
    ParseFunc infix;
    Precedence precedence;
}

Scanner scanner;
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
        parser.current = scanner.scanToken();

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

bool check(TokenType type) {
    return parser.current.type == type;
}

bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
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

void binary(bool canAssign) {
    const TokenType operatorType = parser.previous.type;

    ParseRule rule = getRule(operatorType);
    parsePrecedence(to!Precedence(rule.precedence + 1));

    switch (operatorType) {
        case TokenType.BANG_EQUAL:    emitBytes(OpCode.EQUAL, OpCode.NOT);   break;
        case TokenType.EQUAL_EQUAL:   emitByte(OpCode.EQUAL);                break;
        case TokenType.GREATER:       emitByte(OpCode.GREATER);              break;
        case TokenType.GREATER_EQUAL: emitBytes(OpCode.LESS, OpCode.NOT);    break;
        case TokenType.LESS:          emitByte(OpCode.LESS);                 break;
        case TokenType.LESS_EQUAL:    emitBytes(OpCode.GREATER, OpCode.NOT); break;
        case TokenType.PLUS:          emitByte(OpCode.ADD);                  break;
        case TokenType.MINUS:         emitByte(OpCode.SUBTRACT);             break;
        case TokenType.STAR:          emitByte(OpCode.MULTIPLY);             break;
        case TokenType.SLASH:         emitByte(OpCode.DIVIDE);               break;
        default:
            return;
    }
}

void literal(bool canAssign) {
    const TokenType operatorType = parser.previous.type;
    switch (operatorType) {
        case TokenType.TRUE:  emitByte(OpCode.TRUE);  break;
        case TokenType.FALSE: emitByte(OpCode.FALSE); break;
        case TokenType.NIL:   emitByte(OpCode.NIL);   break;
        default:
            return;
    }
}

void grouping(bool canAssign) {
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
}

void number(bool canAssign) { 
    auto value = to!double(parser.previous.content);
    emitConstant(Value(value));
}

void lox_string(bool canAssign) {
    auto value = Value(
        Obj(to!string(parser.previous.content[1..$-1]))
    );

    emitConstant(value);
}

void namedVariable(Token* name, bool canAssign) {
    auto arg = identifierConstant(name);
    
    if (canAssign && match(TokenType.EQUAL)) {
        expression();
        emitBytes(OpCode.SET_GLOBAL, arg);
    } else {
        emitBytes(OpCode.GET_GLOBAL, arg);
    }
}

void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

void unary(bool canAssign) {
    const TokenType operatorType = parser.previous.type;

    parsePrecedence(Precedence.UNARY);

    switch (operatorType) { 
        case TokenType.MINUS:
            emitByte(OpCode.NEGATE);
            break;
        case TokenType.BANG:
            emitByte(OpCode.NOT);
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
        TokenType.LEFT_PAREN    : ParseRule(&grouping,   null,    Precedence.NONE),
        TokenType.RIGHT_PAREN   : ParseRule(null,        null,    Precedence.NONE),
        TokenType.LEFT_BRACE    : ParseRule(null,        null,    Precedence.NONE), 
        TokenType.RIGHT_BRACE   : ParseRule(null,        null,    Precedence.NONE),
        TokenType.COMMA         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.DOT           : ParseRule(null,        null,    Precedence.NONE),
        TokenType.MINUS         : ParseRule(&unary,      &binary, Precedence.TERM),
        TokenType.PLUS          : ParseRule(null,        &binary, Precedence.TERM),
        TokenType.SEMICOLON     : ParseRule(null,        null,    Precedence.NONE),
        TokenType.SLASH         : ParseRule(null,        &binary, Precedence.FACTOR),
        TokenType.STAR          : ParseRule(null,        &binary, Precedence.FACTOR),
        TokenType.BANG          : ParseRule(&unary,      null,    Precedence.NONE),
        TokenType.BANG_EQUAL    : ParseRule(null,        &binary, Precedence.EQUALITY),
        TokenType.EQUAL         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.EQUAL_EQUAL   : ParseRule(null,        &binary, Precedence.EQUALITY),
        TokenType.GREATER       : ParseRule(null,        &binary, Precedence.COMPARISON),
        TokenType.GREATER_EQUAL : ParseRule(null,        &binary, Precedence.COMPARISON),
        TokenType.LESS          : ParseRule(null,        &binary, Precedence.COMPARISON),
        TokenType.LESS_EQUAL    : ParseRule(null,        &binary, Precedence.COMPARISON),
        TokenType.IDENTIFIER    : ParseRule(&variable,   null,    Precedence.NONE),
        TokenType.STRING        : ParseRule(&lox_string, null,    Precedence.NONE),
        TokenType.NUMBER        : ParseRule(&number,     null,    Precedence.NONE),
        TokenType.AND           : ParseRule(null,        null,    Precedence.NONE),
        TokenType.CLASS         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.ELSE          : ParseRule(null,        null,    Precedence.NONE),
        TokenType.FALSE         : ParseRule(&literal,    null,    Precedence.NONE),
        TokenType.FOR           : ParseRule(null,        null,    Precedence.NONE),
        TokenType.FUN           : ParseRule(null,        null,    Precedence.NONE),
        TokenType.IF            : ParseRule(null,        null,    Precedence.NONE),
        TokenType.NIL           : ParseRule(&literal,    null,    Precedence.NONE),
        TokenType.OR            : ParseRule(null,        null,    Precedence.NONE),
        TokenType.PRINT         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.RETURN        : ParseRule(null,        null,    Precedence.NONE),
        TokenType.SUPER         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.THIS          : ParseRule(null,        null,    Precedence.NONE),
        TokenType.TRUE          : ParseRule(&literal,    null,    Precedence.NONE),
        TokenType.VAR           : ParseRule(null,        null,    Precedence.NONE),
        TokenType.WHILE         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.ERROR         : ParseRule(null,        null,    Precedence.NONE),
        TokenType.EOF           : ParseRule(null,        null,    Precedence.NONE)
    ];
}

void parsePrecedence(Precedence precedence) {
    advance();
    auto prefixRule = getRule(parser.previous.type).prefix;

    if (prefixRule == null) {
        error("Expected expression.");
        return;
    }

    auto canAssign = precedence <= Precedence.ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type).precedence) {
        advance();
        auto infixRule = getRule(parser.previous.type).infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TokenType.EQUAL)) {
        error("Invalid assignment target.");
    }
}

ubyte identifierConstant(Token* name) {
    return makeConstant(Value(Obj(to!string(name.content))));
}

ubyte parseVariable(string errorMessage) {
    consume(TokenType.IDENTIFIER, errorMessage);
    return identifierConstant(parser.previous);
}

void defineVariable(ubyte global) {
    emitBytes(OpCode.DEFINE_GLOBAL, global);
}

ParseRule getRule(TokenType type) {
    return rules[type];
}

void expression() {
    parsePrecedence(Precedence.ASSIGNMENT);
}

void varDeclaration() {
    auto global = parseVariable("Expect variable name.");

    if (match(TokenType.EQUAL)) {
        expression();
    } else { 
        emitByte(OpCode.NIL);
    }

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration");

    defineVariable(global);
}

void expressionStatement() {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    emitByte(OpCode.POP);
}

void printStatement() {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    emitByte(OpCode.PRINT);
}

void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TokenType.EOF) {
        if (parser.previous.type == TokenType.SEMICOLON) return;

        switch (parser.current.type) {
            case TokenType.CLASS:
            case TokenType.FUN:
            case TokenType.VAR:
            case TokenType.FOR:
            case TokenType.IF:
            case TokenType.WHILE:
            case TokenType.PRINT:
            case TokenType.RETURN:
                return;
            
            default:
                // do nothing
        }

        advance();
    }
}

void declaration() {
    if (match(TokenType.VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();    
}

void statement() {
    if (match(TokenType.PRINT)) {
        printStatement();
    } else { 
        expressionStatement();
    }
}

bool compile(string source, Chunk *chunk) {
    initRules();
    scanner = new Scanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TokenType.EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}