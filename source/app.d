import std.stdio;
import common;
import lox_debug;
import value;
import lox_vm;

void main()
{
	initVM();
	Chunk *chunk = new Chunk(0);

	auto constant = chunk.addConstant(Value(1.2));
	chunk.write(OpCode.CONSTANT, 666);
	chunk.write(constant, 666);

	constant = chunk.addConstant(Value(3.4));
	chunk.write(OpCode.CONSTANT, 666);
	chunk.write(constant, 666);

	constant = chunk.addConstant(Value(5.6));
	chunk.write(OpCode.CONSTANT, 666);
	chunk.write(constant, 666);

	chunk.write(OpCode.DIVIDE, 666);

	chunk.write(OpCode.NEGATE, 666);
	chunk.write(OpCode.NEGATE, 666);
	chunk.write(OpCode.NEGATE, 666);

	chunk.write(OpCode.RETURN, 666);

	// for (int i = 0; i < chunk.count; i++) {
	// 	writefln("chunk: %d", chunk.code[i]);
	// }

	// writefln("chunk.count:    %d", chunk.count);
	// writefln("chunk.capacity: %d", chunk.capacity);

	interpret(chunk);

	// writeln("-------------------------------");
	// disassembleChunk(chunk, "main");
}
