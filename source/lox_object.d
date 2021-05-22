module lox_object;

import common;
import value;
import std.variant;

struct Func {
    int arity;
    Chunk* chunk;
    string name;
}

struct Closure {
	Func* func;
}

alias NativeFunc = Value* delegate(int argCount, Value*[] args);

struct Native {
    NativeFunc func;
}

alias Obj = Algebraic!(Closure*, Func*, Native*, string);
