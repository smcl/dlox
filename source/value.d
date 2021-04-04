module value;

import std.stdio;
import std.typecons;
import std.variant;
import common;

alias nil = typeof(null);
alias Value = Algebraic!(bool, nil, double, Obj);
alias Obj = Algebraic!(string);

void writeObject(Obj o) {
    o.visit!(
        (string s) => writef("%s", s)
    );
}

void writeValue(Value v) {
    v.visit!(
        (double d) => writef("%g", d),
        (bool b) { 
            if (b) { 
                write("true");
            } else {
                write("false");
            }
        },
        (nil) => write("nil"),
        (Obj o) => writeObject(o)
    );
}

bool valuesEqual(Value a, Value b) {
    return a.visit!(
        (double aNum) => 
            b.visit!(
                (double bNum) => aNum == bNum,
                (_) => false
            ),
            
        (bool aBool) =>
            b.visit!(
                (bool bBool) => aBool == bBool,
                (_) => false
            ),

        (nil n) => false,

        // TODO: This is wrong and needs to be filled out
        (Obj aObj) => b.visit!(
            (Obj bObj) => objectEqual(aObj, bObj),
            (_) => false
        )
    );
}

bool objectEqual(Obj a, Obj b) {
    return a.visit!(
        (string aStr) => b.visit!(
            (string bStr) { 
                writefln("{%s} == {%s}", aStr, bStr);
                return aStr == bStr;
            },
            // (string bStr) => aStr == bStr,
            (_) => false
        ),
        (_) => false
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

bool isFalsey(Value v) {
    return v.visit!(
        (double _) => false,
        (bool b)   => !b,
        (nil _)    => true,
        (Obj o) => o.visit!(
            (string s) => s.length == 0
        )
    );
}