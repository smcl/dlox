module lox_debug;

import common;
import std.stdio;
import value;

int constantInstruction(string name, Chunk *chunk, int offset) {
    const ubyte constant = chunk.code[offset + 1];
    writef("%-16s %4d '", name, constant);
    printValue(chunk.constants.values[constant]);
    writef("'\n");
    return offset + 2;
}

int simpleInstruction(string name, int offset) {
    writef("%s\n", name);
    return offset + 1;
}

int disassembleInstruction(Chunk *chunk, int offset) {
    writef("%04d ", offset);

    if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
        writef("   | ");
    } else {
        writef("%4d ", chunk.lines[offset]);
    }

    ubyte instruction = chunk.code[offset];

    switch (instruction) {
        case OpCode.OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OpCode.OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
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