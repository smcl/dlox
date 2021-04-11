module lox_object;

import common;
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

alias Obj = Algebraic!(string, Func);