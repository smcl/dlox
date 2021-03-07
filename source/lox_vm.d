module lox_vm;

import common;
import value;
import std.stdio;
import std.variant;
import lox_debug;

const size_t STACK_MAX = 256;

struct VM {
    Chunk *chunk;
    int ip; /* maybe uint or size_t? */
    Value[STACK_MAX] stack;
    int sp;
}

enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR
}

VM vm;

void initVM() {
    resetStack();
}

void resetStack() {
    vm.sp = 0;
}

void runtimeError(string format, ...) {
    stderr.writefln(format, _arguments);

    auto line = vm.chunk.lines[vm.ip - 1];
    stderr.writefln("[line %d] in script\n", line);
}

InterpretResult interpret(Chunk *chunk) {
    vm.chunk = chunk;
    vm.ip = 0;

    return run();
}

void push(Value value) {
    vm.stack[vm.sp] = value;
    vm.sp += 1;
}

Value pop() {
    vm.sp -= 1;
    return vm.stack[vm.sp];
}

bool binary_op(Value function(double lhs, double rhs) op) {
    const Value b = pop();
    const Value a = pop();

    const bool ok = a.visit!(
        (double aNum) =>
            b.visit!(
                (double bNum) {
                    const auto res = op(aNum, bNum);
                    push(res);
                    return true;
                },
                (_) => false
            ),
        (_) => false
    );

    if (!ok) {
        runtimeError("Both operands must be numbers.");
        return false;
    }
    
    return true;
}

bool isFalsey(Value v) {
    return v.visit!(
        (double _) => false,
        (bool b)   => !b,
        (nil _)    => true
    );
}

InterpretResult run() {
    while (true) {
        debug {
            writef("          ");
            for (int i = 0; i < vm.sp; i++) {
                writef("[ ");
                writeValue(vm.stack[i]);
                writef(" ]");
            }
            writef("\n");
            disassembleInstruction(vm.chunk, vm.ip);
        }

        const ubyte instr = vm.chunk.code[vm.ip++];

        switch (instr) {
            case OpCode.CONSTANT:
                auto constant_index = vm.chunk.code[vm.ip++];
                auto constant = vm.chunk.constants.values[constant_index];
                push(constant);
                break;
            case OpCode.TRUE:
                push(Value(true));
                break;
            case OpCode.FALSE:
                push(Value(false));
                break;
            case OpCode.EQUAL:
                Value b = pop();
                Value a = pop();
                push(Value(valuesEqual(a, b)));
                break;
            case OpCode.NIL:
                push(Value(null));
                break;
            case OpCode.GREATER:
                const auto ok = binary_op((double a, double b) => cast(Value)(a > b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }
                break;
            case OpCode.LESS:
                const auto ok = binary_op((double a, double b) => cast(Value)(a < b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }
                break;
            case OpCode.ADD:
                const auto ok = binary_op((double a, double b) => cast(Value)(a + b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }
                break;
            case OpCode.SUBTRACT:
                const auto ok = binary_op((double a, double b) => cast(Value)(a - b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }                
                break;
            case OpCode.MULTIPLY:
                const auto ok = binary_op((double a, double b) => cast(Value)(a * b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }                
                break;
            case OpCode.DIVIDE:
                const auto ok = binary_op((double a, double b) => cast(Value)(a / b));
                if (!ok) {
                    return InterpretResult.RUNTIME_ERROR;
                }
                break;
            case OpCode.NEGATE:
                const auto operand = pop();
                const auto ok = operand.visit!(
                    (double d) { 
                        push(Value(-d));
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
                push(Value(isFalsey(pop())));
                break;
            case OpCode.RETURN: 
                writeValue(pop());
                writef("\n");
                return InterpretResult.OK;
            default:
                return InterpretResult.RUNTIME_ERROR;
        }
    }
}



unittest {
    initVM();

    auto chunk = new Chunk(0);
	auto boolConstant = chunk.addConstant(Value(true));
	chunk.write(OpCode.CONSTANT, 666);
	chunk.write(boolConstant, 666);

    chunk.write(OpCode.NEGATE, 666);

    const auto res = interpret(chunk);

    assert(res == InterpretResult.RUNTIME_ERROR);
}