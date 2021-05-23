module lox_object;

import common;
import value;
import std.variant;

struct Func {
    int arity;
    int upvalueCount;
    Chunk* chunk;
    string name;
}

struct Closure {
	Func* func;
    ObjUpvalue*[] upvalues;
    int upvalueCount;

	this(Func* func) {
		this.func = func;
        this.upvalueCount = func.upvalueCount;

        this.upvalues = new ObjUpvalue*[this.upvalueCount];
		for (int i=0; i<func.upvalueCount; i++) {
			this.upvalues[i] = null;
		}
	}
}

struct ObjUpvalue {
    Value* location;
}

alias NativeFunc = Value* delegate(int argCount, Value*[] args);

struct Native {
    NativeFunc func;
}

alias Obj = Algebraic!(ObjUpvalue*, Closure*, Func*, Native*, string);
