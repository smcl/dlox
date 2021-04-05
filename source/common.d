module common;

import value;
import std.conv;

enum OpCode: ubyte {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    PRINT,
    RETURN
}

int grow_capacity(int capacity) {
    if (capacity == 0) {
        return 8;
    }

    return capacity * 2;
}

unittest {
    assert(8 == grow_capacity(0));
    assert(2 == grow_capacity(1));
    assert(16 == grow_capacity(8));
}

T[] grow_array(T)(T[] array, ulong oldCount, ulong newCount) {
    T[] newArray = new T[](newCount);
    newArray[0..oldCount] = array;
    return newArray;        
}

unittest {
    ubyte[] originalArray = [ 0, 1, 2, 3, 4, 10 ];
    auto grownArray = grow_array!ubyte(originalArray, originalArray.length, originalArray.length + 2);
    assert(2 + originalArray.length == grownArray.length);
    for (int i = 0; i < originalArray.length; i++) {
        assert(originalArray[i] == grownArray[i]);
    }
}

struct Chunk {
    int count;
    int capacity;
    ubyte[] code;
    ValueArray *constants;
    int[] lines;

    this(int initialCapacity) {
        this.count = 0;
        this.capacity = initialCapacity;
        this.code = new ubyte[initialCapacity];
        this.constants = new ValueArray(initialCapacity);
        this.lines = new int[initialCapacity];
    }

    void write(ubyte b, int line) {
        if (this.capacity < this.count + 1) {
            const int oldCapacity = this.capacity;
            this.capacity = grow_capacity(oldCapacity);
            this.code = grow_array!ubyte(this.code, oldCapacity, this.capacity);
            this.lines = grow_array!int(this.lines, oldCapacity, this.capacity);
        }

        this.code[this.count] = b;
        this.lines[this.count] = line;
        this.count++;
    }

    ubyte addConstant(Value v) {
        this.constants.write(v);
        return to!ubyte(this.constants.count - 1);
    }
}