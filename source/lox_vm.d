module lox_vm;

import common;
import value;
import std.stdio;
import std.variant;
import lox_debug;

const size_t STACK_MAX = 256;

enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR
}

class VM {
    Chunk* chunk;
    int ip; /* maybe uint or size_t? */
    Value[STACK_MAX] stack;
    int sp;

    this(){
        this.resetStack();
    }

    InterpretResult run() {
        while (true) {
            debug {
                writef("          ");
                for (int i = 0; i < this.sp; i++) {
                    writef("[ ");
                    writeValue(this.stack[i]);
                    writef(" ]");
                }
                writef("\n");
                disassembleInstruction(this.chunk, this.ip);
            }

            const ubyte instr = this.chunk.code[this.ip++];

            switch (instr) {
                case OpCode.CONSTANT:
                    auto constant_index = this.chunk.code[this.ip++];
                    auto constant = this.chunk.constants.values[constant_index];
                    push(constant);
                    break;
                case OpCode.TRUE:
                    push(Value(true));
                    break;
                case OpCode.FALSE:
                    push(Value(false));
                    break;
                case OpCode.EQUAL:
                    Value b = this.pop();
                    Value a = this.pop();
                    this.push(Value(valuesEqual(a, b)));
                    break;
                case OpCode.NIL:
                    this.push(Value(null));
                    break;
                case OpCode.GREATER:
                    const auto ok = this.binary_op( 
                        (double a, double b) => cast(Value)(a > b), 
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.LESS:
                    const auto ok = this.binary_op(
                        (double a, double b) => cast(Value)(a < b),
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.ADD:
                    const auto ok = this.binary_op(
                        (double a, double b) => cast(Value)(a + b),
                        (Obj a,    Obj b)    => objectAdd(a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.SUBTRACT:
                    const auto ok = this.binary_op(
                        (double a, double b) => cast(Value)(a - b),
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.MULTIPLY:
                    const auto ok = this.binary_op(
                        (double a, double b) => cast(Value)(a * b), 
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.DIVIDE:
                    const auto ok = this.binary_op(
                        (double a, double b) => cast(Value)(a / b),
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.NEGATE:
                    const auto operand = this.pop();
                    const auto ok = operand.visit!(
                        (double d) { 
                            this.push(Value(-d));
                            return true;
                        },
                        (_) {
                            runtimeError("Operand must be a number.");
                            return false;
                        }
                    );

                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    break;
                case OpCode.NOT:
                    this.push(Value(isFalsey(this.pop())));
                    break;
                case OpCode.RETURN: 
                    writeValue(this.pop());
                    writef("\n");
                    return InterpretResult.OK;
                default:
                    return InterpretResult.RUNTIME_ERROR;
            }
        }
    }

    void resetStack() {
        this.sp = 0;
    }    

    void runtimeError(string format, ...) {
        stderr.writefln(format, _arguments);

        auto line = this.chunk.lines[this.ip - 1];
        stderr.writefln("[line %d] in script\n", line);
    }

    void push(Value value) {
        this.stack[this.sp] = value;
        this.sp += 1;
    }

    Value pop() {
        this.sp -= 1;
        return this.stack[this.sp];
    }

    bool binary_op(
        Value delegate(double lhs, double rhs) double_op,
        Value delegate(Obj lhs, Obj rhs) object_op) {

        const Value b = this.pop();
        const Value a = this.pop();

        const bool ok = a.visit!(
            (double aNum) =>
                b.visit!(
                    (double bNum) {
                        const auto res = double_op(aNum, bNum);
                        this.push(res);
                        return true;
                    },
                    (_) => false
                ),
            (Obj aObj) => 
                b.visit!(
                    (Obj bObj) {
                        const auto res = object_op(aObj, bObj);
                        this.push(res);
                        return true;
                    },
                    (_) => false
                ),
            (_) => false
        );

        if (!ok) {
            this.runtimeError("Both operands must be numbers or strings.");
            return false;
        }
        
        return true;
    }
}

Value objectAdd(Obj a, Obj b) {
    return a.visit!(
        (string aStr) => 
            b.visit!(
                (string bStr) => Value(Obj(aStr ~ bStr))
                /* no other types in Obj */
            ),
        /* no other types in Obj */
    );
}

Value doubleOpOnly(VM vm, Obj a, Obj b) {
    vm.runtimeError("This is a .");
    return Value(null);
}