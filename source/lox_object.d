module lox_object;

import common;
import value;
import std.variant;

// ObjString* copyString(const char* chars, int length) {
//   char* heapChars = ALLOCATE(char, length + 1);
//   memcpy(heapChars, chars, length);
//   heapChars[length] = '\0';

//   return allocateString(heapChars, length);
// }


// static Obj* allocateObject(size_t size, ObjType type) {
//   Obj* object = (Obj*)reallocate(NULL, 0, size);
//   object->type = type;
//   return object;
// }

struct Func {
    int arity;
    Chunk* chunk;
    string name;
}

alias NativeFunc = Value* delegate(int argCount, Value*[] args);

struct Native {
    NativeFunc func;
}

alias Obj = Algebraic!(Func*, Native*, string);
