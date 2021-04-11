module lox_vm;

import common;
import value;
import std.stdio;
import std.variant;
import lox_debug;
import lox_object;

const size_t FRAMES_MAX = 64;
const size_t STACK_MAX = FRAMES_MAX * 255;

struct CallFrame {
    Func* func;
    ubyte ip;
    Value[] slots;
}

enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR
}

class VM {
    CallFrame[FRAMES_MAX] frames;
    int frameCount;

    Value[STACK_MAX] stack;
    int sp;
    Value[string] globals;

    this(){
        this.resetStack();
    }

    InterpretResult run() {
        auto frame = &this.frames[this.frameCount -1];

        while (true) {
            debug {
                writef("          ");
                for (int i = 0; i < this.sp; i++) {
                    writef("[ ");
                    writeValue(this.stack[i]);
                    writef(" ]");
                }
                writef("\n");
                disassembleInstruction(frame.func.chunk, frame.ip);
            }

            const ubyte instr = readByte(frame);
            switch (instr) {
                case OpCode.CONSTANT:
                    const auto constant = readConstant(frame);                    
                    push(constant);
                    break;
                case OpCode.TRUE:
                    push(Value(true));
                    break;
                case OpCode.FALSE:
                    push(Value(false));
                    break;
                case OpCode.POP:
                    pop();
                    break;
                case OpCode.GET_LOCAL:
                    const auto slot = readByte(frame);
                    this.push(frame.slots[slot]);
                    break;
                case OpCode.SET_LOCAL:
                    const auto slot = readByte(frame);
                    frame.slots[slot] = this.peek(0);
                    break;
                case OpCode.GET_GLOBAL:
                    const auto name = readString(frame);
                    if (!(name in this.globals)) {
                        runtimeError(frame, "Undefined variable '%s'", name);
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    this.push(this.globals[name]);
                    break;
                case OpCode.DEFINE_GLOBAL:
                    const auto name = readString(frame);
                    this.globals[name] = this.peek(0);
                    this.pop();
                    break;
                case OpCode.SET_GLOBAL:
                    const auto name = readString(frame);
                    if (!(name in this.globals)) {
                        runtimeError(frame, "Undefined variable '%s'", name);
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    this.globals[name] = this.peek(0);
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
                        frame,
                        (double a, double b) => cast(Value)(a > b), 
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.LESS:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => cast(Value)(a < b),
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.ADD:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => cast(Value)(a + b),
                        (Obj a,    Obj b)    => objectAdd(a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.SUBTRACT:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => cast(Value)(a - b),
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.MULTIPLY:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => cast(Value)(a * b), 
                        (Obj a,    Obj b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.DIVIDE:
                    const auto ok = this.binary_op(
                        frame,
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
                            runtimeError(frame, "Operand must be a number.");
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
                case OpCode.PRINT: 
                    writeValue(pop());
                    writeln("");
                    break;
                case OpCode.JUMP:
                    const ushort offset = readShort(frame);
                    frame.ip += offset;
                    break;
                case OpCode.JUMP_IF_FALSE:
                    const ushort offset = readShort(frame);
                    if (isFalsey(peek(0))) {
                        frame.ip += offset;
                    }
                    break;
                case OpCode.LOOP:
                    const auto offset = readShort(frame);
                    frame.ip -= offset;
                    break;
                case OpCode.RETURN: 
                    return InterpretResult.OK;
                default:
                    return InterpretResult.RUNTIME_ERROR;
            }
        }
    }

    ubyte readByte(CallFrame* frame) {
        return frame.func.chunk.code[frame.ip++];
    }

    ushort readShort(CallFrame* frame) {
        frame.ip += 2;
        const ushort hi = frame.func.chunk.code[frame.ip - 2] << 8;
        const ushort lo = frame.func.chunk.code[frame.ip - 1];
        return hi | lo;
    }

    string readString(CallFrame* frame) { 
        const auto index = readByte(frame);
        const auto value = frame.func.chunk.constants.values[index];
        return *(value.peek!Obj().peek!(string));
    }

    Value readConstant(CallFrame* frame) {
        const auto constant_index = readByte(frame);
        return frame.func.chunk.constants.values[constant_index];
    }

    void resetStack() {
        this.sp = 0;
        this.frameCount = 0;
    }    

    void runtimeError(CallFrame *frame, string format, ...) {
        stderr.writefln(format, _arguments);

        auto line = frame.func.chunk.lines[frame.ip - 1];
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

    Value peek(int distance) {
        return this.stack[this.sp - 1 - distance];
    }

    bool binary_op(
        CallFrame *frame,
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
            this.runtimeError(frame, "Both operands must be numbers or strings.");
            return false;
        }
        
        return true;
    }
}

Value objectAdd(Obj a, Obj b) {
    return a.visit!(
        (string aStr) => 
            b.visit!(
                (string bStr) => Value(Obj(aStr ~ bStr)),
                // TODO: addition of funcs doesn't make sense
                //       we'll need to handle this better at
                //       some point in the future
                (Func _) => Value(null)
            ),

        // TODO: as above - adding a Func doesn't make sense
        (Func _) => Value(null)
    );
}

Value doubleOpOnly(VM vm, Obj a, Obj b) {
    // TODO: move this back into VM
    vm.runtimeError(null, "This is a wendys.");
    return Value(null);
}