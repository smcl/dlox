import std.file;
import std.stdio;
import std.utf;
import common;
import lox_debug;
import value;
import lox_vm;
import core.stdc.stdlib;
import lox_compiler;

int main(string[] args)
{
	if (args.length == 1) {
		repl();
	} else if (args.length == 2) {
		runFile(args[1]);
	} else { 
		stderr.writeln("Usage: dlox [path]");
		return 64;		
	}

	return 0;
}


void repl() {

	auto vm = new VM();

	while (true) {
		writef("> ");
		auto line = readln();
		
		if (line.length == 1) {
			writeln();
			break;
		}

		interpret(&vm, line);
	}
}

void runFile(string path) {
	auto vm = new VM();
	auto source = readFile(path);
	const auto result = interpret(&vm, source);

	if (result == InterpretResult.COMPILE_ERROR) {
		exit(65);
	}

	if (result == InterpretResult.RUNTIME_ERROR) {
		exit(70);
	}
}

InterpretResult interpret(VM* vm, string source) {
	Chunk* chunk = new Chunk(8);
	
	if (!compile(source, chunk)) {
		return InterpretResult.COMPILE_ERROR;
	}

	vm.chunk = chunk;
	vm.ip = 0;
	
	return vm.run();
}


string readFile(string path) {
	try {
		return readText(path);
	} catch (FileException e) {
		stderr.writefln("Error when reading file \"%s\".");
		exit(74);
	} catch (UTFException e) {
		stderr.writefln("Weird-ass unicode situation in file \"%s\".");
		exit(74);
	}
	
	// shouldn't reach this
	return "";
}

void testInterpreter() {
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
}  

unittest { 
    auto vm = new VM();

    auto chunk = new Chunk(0);
    auto boolConstant = chunk.addConstant(Value(true));
    chunk.write(OpCode.CONSTANT, 666);
    chunk.write(boolConstant, 666);

    chunk.write(OpCode.NEGATE, 666);

    const auto res = interpret(&vm, chunk);

    assert(res == InterpretResult.RUNTIME_ERROR);
}