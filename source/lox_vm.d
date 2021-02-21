module lox_vm;

import common;
import value;
import std.stdio;
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

void binary_op(double function(Value lhs, Value rhs) op) {
    Value b = pop();
    Value a = pop();
    Value res = cast(Value)op(a, b);
    push(res);
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
            case OpCode.ADD:
                binary_op((Value a, Value b) => (a + b));
                break;
            case OpCode.SUBTRACT:
                binary_op((Value a, Value b) => (a - b));
                break;
            case OpCode.MULTIPLY:
                binary_op((Value a, Value b) => (a * b));
                break;
            case OpCode.DIVIDE:
                binary_op((Value a, Value b) => (a / b));
                break;                                
            case OpCode.NEGATE:
                Value val = -(pop());
                push(val);
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