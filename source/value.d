module value;

import std.stdio;
import std.typecons;
import common;

alias Value = Typedef!double;

void printValue(Value v) {
    writef("%g", v);
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
