module lox_debug;

import common;
import std.stdio;
import value;

int constantInstruction(string name, Chunk *chunk, int offset) {
    const ubyte constant = chunk.code[offset + 1];
    writef("%-16s %4d '", name, constant);
    writeValue(chunk.constants.values[constant]);
    writef("'\n");

    // writef("\t offset = %d\n", offset);
    // writef("\t constant = %d\n", constant);

    return offset + 2;
}

int simpleInstruction(string name, int offset) {
    writef("%s\n", name);
    return offset + 1;
}

int disassembleInstruction(Chunk *chunk, int offset) {
    writef("%04d ", offset);

    if (false && offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
        writef("   | ");
    } else {
        writef("%4d ", chunk.lines[offset]);
    }

    ubyte instruction = chunk.code[offset];

    switch (instruction) {
        case OpCode.CONSTANT:
            return constantInstruction("CONSTANT", chunk, offset);
        case OpCode.TRUE:
            return simpleInstruction("TRUE", offset);            
        case OpCode.FALSE:            
            return simpleInstruction("FALSE", offset);
        case OpCode.POP:
            return simpleInstruction("POP", offset);
        case OpCode.GET_GLOBAL:
            return constantInstruction("GET_GLOBAL", chunk, offset);
        case OpCode.DEFINE_GLOBAL:
            return constantInstruction("DEFINE_GLOBAL", chunk, offset);
        case OpCode.SET_GLOBAL:
            return constantInstruction("SET_GLOBAL", chunk, offset);
        case OpCode.EQUAL:
            return simpleInstruction("EQUAL", offset);
        case OpCode.GREATER:
            return simpleInstruction("GREATER", offset);
        case OpCode.LESS:
            return simpleInstruction("LESS", offset);
        case OpCode.NIL:            
            return simpleInstruction("NIL", offset);
        case OpCode.ADD:
            return simpleInstruction("ADD", offset);
        case OpCode.SUBTRACT:
            return simpleInstruction("SUBTRACT", offset);
        case OpCode.MULTIPLY:
            return simpleInstruction("MULTIPLY", offset);
        case OpCode.DIVIDE:
            return simpleInstruction("DIVIDE", offset);
        case OpCode.NOT:
            return simpleInstruction("NOT", offset);
        case OpCode.NEGATE:
            return simpleInstruction("NEGATE", offset);
        case OpCode.PRINT:
            return simpleInstruction("PRINT", offset);
        case OpCode.RETURN:
            return simpleInstruction("RETURN", offset);
        default:
            writefln("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

void disassembleChunk(Chunk *chunk, string name) {
    writef("== %s ==\n", name);

    for (int offset = 0; offset < chunk.count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}