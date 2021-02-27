module lox_compiler;

import std.stdio;
import std.conv;

import lox_scanner;

void compile(string source) {
    initScanner(source);

    int line = -1;

    while (true) {
        auto token = scanToken();

        if (token.line != line) {
            writef("%4d ", token.line);
            line = token.line;
        } else {
            writef("    | ");
        }

        writefln("%2d '%s'", token.type, to!string(token.content));

        if (token.type == TokenType.EOF) break;
    }
}