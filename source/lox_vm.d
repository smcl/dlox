module lox_vm;

import common;
import value;
import std.stdio;
import std.variant;
import lox_debug;
import lox_object;
import std.conv;
import std.datetime.systime;

const size_t FRAMES_MAX = 64;
const size_t STACK_MAX = FRAMES_MAX * 255;

struct CallFrame {
    Closure* closure;
    ubyte ip;
    int fp;
    Value*[] slots;
}

class VM {
    CallFrame*[] frames;
    int frameCount;

    Value*[] stack;
    int sp;
    Value*[string] globals;

    this(){
        this.resetStack();
        this.frames = new CallFrame*[FRAMES_MAX];
        this.stack = new Value*[STACK_MAX];

        for (auto i = 0; i < FRAMES_MAX; i++){
            this.frames[i] = new CallFrame();
		}

		for (auto i = 0; i < STACK_MAX; i++){
            this.stack[i] = new Value();
		}

        this.defineNative("sean", &this.seanNative);
        this.defineNative("clock", &this.clockNative);
    }

    InterpretResult run() {
        auto frame = this.frames[this.frameCount - 1];

        while (true) {
			debug {
			   writef("          ");
			   for (int i = 0; i < this.sp; i++) {
			       writef("[ ");
			       writeValue(this.stack[i]);
			       writef(" ]");
			   }
			   writef("\n");
			   disassembleInstruction(frame.closure.func.chunk, frame.ip);
			}

            const ubyte instr = readByte(frame);
            switch (instr) {
                case OpCode.CONSTANT:
                    auto constant = readConstant(frame);                    
                    push(constant);
                    break;
                case OpCode.TRUE:
                    push(new Value(true));
                    break;
                case OpCode.FALSE:
                    push(new Value(false));
                    break;
                case OpCode.POP:
                    pop();
                    break;
                case OpCode.GET_LOCAL:
                    auto slot = readByte(frame);
                    this.push(frame.slots[slot]);
                    break;
                case OpCode.SET_LOCAL:
                    auto slot = readByte(frame);
                    frame.slots[slot] = this.peek(0);
                    break;
                case OpCode.GET_GLOBAL:
                    auto name = readString(frame);
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
                case OpCode.GET_UPVALUE:
                    auto slot = readByte(frame);
                    push(frame.closure.upvalues[slot].location);
                    break;
                case OpCode.SET_UPVALUE:
                    auto slot = readByte(frame);
                    frame.closure.upvalues[slot].location = this.peek(0);
                    break;

                case OpCode.EQUAL:
                    auto b = this.pop();
                    auto a = this.pop();
                    this.push(new Value(valuesEqual(a, b)));
                    break;
                case OpCode.NIL:
                    this.push(new Value(null));
                    break;
                case OpCode.GREATER:
                    const auto ok = this.binary_bool_op( 
                        frame,
                        (double a, double b) => a > b, 
                        (Obj* a,    Obj* b)    => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.LESS:
                    const auto ok = this.binary_bool_op(
                        frame,
                        (double a, double b) => a < b,
                        (Obj* a,    Obj* b)  => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.ADD:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => a + b,
                        (Obj* a,    Obj* b)    => objectAdd(a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.SUBTRACT:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => a - b,
                        (Obj* a,    Obj* b)  => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.MULTIPLY:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => a * b, 
                        (Obj* a,    Obj* b)  => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }                
                    break;
                case OpCode.DIVIDE:
                    const auto ok = this.binary_op(
                        frame,
                        (double a, double b) => a / b,
                        (Obj* a,    Obj* b)  => doubleOpOnly(this, a, b));
                    if (!ok) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case OpCode.NEGATE:
                    auto operand = this.pop();
                    auto ok = (*operand).visit!(
                        (double d) { 
                            this.push(new Value(-d));
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
                    this.push(new Value(isFalsey(pop())));
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
                case OpCode.CALL: 
                    auto argCount = readByte(frame);
                    if (!callValue(peek(argCount), argCount)) {
                        return InterpretResult.RUNTIME_ERROR;
					}
                    frame = this.frames[this.frameCount - 1];
					break;
                case OpCode.CLOSURE:
                    auto funcObj = *(readConstant(frame).peek!(Obj*)());
                    auto closure = new Closure(*funcObj.peek!(Func*)());
                    push(new Value(new Obj(closure)));

                    for (auto i = 0; i < closure.upvalueCount; i++) {
                        auto isLocal = readByte(frame);
                        auto index = readByte(frame);

                        if (isLocal) {
                            closure.upvalues[i] = captureUpvalue(frame.slots[index]);
						} else { 
                            closure.upvalues[i] = frame.closure.upvalues[index];
						}
					}

                    break;
                case OpCode.RETURN: 
                    auto result = this.pop();
                    this.frameCount--;
                    if (this.frameCount == 0) {
                        this.pop();
                        return InterpretResult.OK;
                    }

                    this.sp = frame.fp;

                    frame = this.frames[this.frameCount - 1];
                    this.push(result);
                    break;
                default:
                    return InterpretResult.RUNTIME_ERROR;
            }
        }
    }

    ubyte readByte(CallFrame* frame) {
        return frame.closure.func.chunk.code[frame.ip++];
    }

    ushort readShort(CallFrame* frame) {
        frame.ip += 2;
        const ushort hi = frame.closure.func.chunk.code[frame.ip - 2] << 8;
        const ushort lo = frame.closure.func.chunk.code[frame.ip - 1];
        return hi | lo;
    }

    string readString(CallFrame* frame) { 
        ubyte index = readByte(frame);
        Value* value = frame.closure.func.chunk.constants.values[index];
        auto obj = (*value).peek!(Obj*)();
		return *(*obj).peek!(string);
    }

    Value* readConstant(CallFrame* frame) {
        auto constant_index = readByte(frame);
        return frame.closure.func.chunk.constants.values[constant_index];
    }

    void resetStack() {
        this.sp = 0;
        this.frameCount = 0;
    }    

    void runtimeError(T...)(CallFrame *errorFrame, string format, T args) {        
        auto frame = errorFrame != null 
            ? errorFrame
            : this.frames[this.frameCount -1];

        stderr.writefln(format, args);

        for (auto i = this.frameCount - 1; i >= 0; i--) {
            auto func = frame.closure.func;
            auto instruction = frame.ip - 1;
            stderr.writef("[line %d] in ", func.chunk.lines[instruction]);
            if (func.name == null) {
                stderr.writeln("script");
            } else {
                stderr.writefln("%s()\n", to!string(func.name));
            }
        }

		//auto line = frame.func.chunk.lines[frame.ip - 1];
		//stderr.writefln("[line %d] in script\n", line);
    }

    void defineNative(string name, NativeFunc fun) {
        this.globals[name] = new Value(new Obj(new Native(fun)));
    }

    void push(Value* value) {
        this.stack[this.sp] = value;
        this.sp += 1;
    }

    Value* pop() {
        this.sp -= 1;
        return this.stack[this.sp];
    }

    Value* peek(int distance) {
        return this.stack[this.sp - 1 - distance];
    }

    bool call(Closure* closure, int argCount) {
        if (argCount != closure.func.arity) {
            runtimeError(null, "Expected %d arguments but got %d", closure.func.arity, argCount);
            return false;
        }

        if (frameCount == FRAMES_MAX) {
            runtimeError(null, "Stack overflow");
            return false;
        }

        auto frame = this.frames[this.frameCount++];
        frame.closure = closure;
        frame.ip = 0;
        frame.fp = this.sp - argCount - 1;
        frame.slots = this.stack[this.sp - argCount - 1..$];

        return true;
    }

    bool callValue(Value* callee, int argCount) {
        auto res = (*callee).visit!(
            (Obj* obj) => (*obj).visit!(
                (Closure* closure) {
                    return this.call(closure, argCount);
                },
                (Native* fun) {
                    auto res = fun.func(argCount, this.stack[this.sp - argCount - 1..this.sp]);
                    this.push(res);
                    return true;
                },
                (_) => false
            ),
            (_) => false
        );

        return res;
	}

    ObjUpvalue* captureUpvalue(Value* local) {
        auto createdUpvalue = new ObjUpvalue(local);
        return createdUpvalue;
	}

    // fuuuuuj
    bool binary_bool_op(CallFrame *frame, bool delegate(double lhs, double rhs) double_op, Obj* delegate(Obj* lhs, Obj* rhs) object_op) {
		auto b = this.pop();
        auto a = this.pop();

        const bool ok = (*a).visit!(
									(double aNum) =>
									(*b).visit!(
												(double bNum) {
													auto res = double_op(aNum, bNum);
													this.push(new Value(res));
													return true;
												},
												(_) => false
													),
									(Obj* aObj) => 
									(*b).visit!(
												(Obj* bObj) {
													auto res = object_op(aObj, bObj);
													this.push(new Value(res));
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

    // bleheheheheheh
    bool binary_op(
        CallFrame *frame,
        double delegate(double lhs, double rhs) double_op,
        Obj* delegate(Obj* lhs, Obj* rhs) object_op) {

        auto b = this.pop();
        auto a = this.pop();

        const bool ok = (*a).visit!(
            (double aNum) =>
                (*b).visit!(
                    (double bNum) {
                        auto res = double_op(aNum, bNum);
                        this.push(new Value(res));
                        return true;
                    },
                    (_) => false
                ),
            (Obj* aObj) => 
                (*b).visit!(
                    (Obj* bObj) {
                        auto res = object_op(aObj, bObj);
                        this.push(new Value(res));
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

    // natives
    Value *seanNative(int argCount, Value*[] args) {
        return new Value(new Obj("Hello, from sean"));
    }

    Value *clockNative(int argCount, Value*[] args) {
        auto clock = Clock.currTime().toUnixTime;
        writefln("%d", clock);
        return new Value(to!double(clock));
    }
}

Obj* objectAdd(Obj* a, Obj* b) {
    return (*a).visit!(
        (string aStr) => 
            (*b).visit!(
                (string bStr) => new Obj(aStr ~ bStr),
                // TODO: addition of funcs doesn't make sense
                //       we'll need to handle this better at
                //       some point in the future
                (Func* _) => null,
                (Native* _) => null,
				(Closure* _) => null,
                (ObjUpvalue* _) => null /* TODO: I think this is PROBABLY wrong */
            ),

        // TODO: as above - adding a Func/Native doesn't make sense
        (Func* f) => null,
        (Native* _) => null,
        (Closure* _) => null,
        (ObjUpvalue* _) => null /* TODO: I think this is PROBABLY wrong */
    );
}

Obj* doubleOpOnly(VM vm, Obj* a, Obj* b) {
    // TODO: move this back into VM
    vm.runtimeError(null, "This is a wendys.");
    return null;
}