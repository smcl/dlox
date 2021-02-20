import std.stdio;
import common;
import lox_debug;
import value;

void main()
{
	Chunk *chunk = new Chunk(0);


	auto constant = chunk.addConstant(Value(1.2));
	chunk.write(OpCode.OP_CONSTANT, 666);
	chunk.write(constant, 666);

	chunk.write(OpCode.OP_RETURN, 666);

	for (int i = 0; i < chunk.count; i++) {
		writefln("chunk: %d", chunk.code[i]);
	}

	writefln("chunk.count:    %d", chunk.count);
	writefln("chunk.capacity: %d", chunk.capacity);

	disassembleChunk(chunk, "main");
}
