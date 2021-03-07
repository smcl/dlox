module value;

import std.stdio;
import std.typecons;
import std.variant;
import common;

// TODO: don't know if this is necessary with the sum-typed
//       Value that I'm using
enum ValueType {
    BOOL,
    NIL,
    NUMBER
};

alias nil = typeof(null);

alias Value = Algebraic!(bool, nil, double);

ValueType getType(Value v) {
    return v.visit!(
        (double d) => ValueType.NUMBER,
        (bool b)   => ValueType.BOOL,
        (nil b)    => ValueType.BOOL
    );
}

bool isBool(Value v) {
    return getType(v) == ValueType.BOOL;
}

bool isNumber(Value v) {
    return getType(v) == ValueType.NUMBER;
}

void writeValue(Value v) {
    v.visit!(
        (double d) => writef("%g", d),
        (bool b) { 
            if (b) { 
                writef("true");
            } else {
                writef("false");
            }
        },
        (nil) => writef("nil")
    );
}

bool valuesEqual(Value a, Value b) {
    return a.visit!(
        (double aNum) => 
            b.visit!(
                (double bNum) => aNum == bNum,
                (bool _) => false,
                (nil _) => false
            ),
        (bool aBool) =>
            b.visit!(
                (double _) => false,
                (bool bBool) => aBool == bBool,
                (nil _) => false
            ),
        (nil n) => false
    );
}

struct ValueArray {
    int capacity;
    int count;
    Value[] values;

    this(int capacity) {
        this.capacity = 0;
        this.count = 0;
        this.values = null;
    }

    void write(Value value) {
        if (this.capacity < this.count + 1) {
            const int oldCapacity = this.capacity;
            this.capacity = grow_capacity(oldCapacity);
            this.values = grow_array!Value(this.values, oldCapacity, this.capacity);
        }

        this.values[this.count] = value;
        this.count++;
    }
}
