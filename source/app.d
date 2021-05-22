module app;

import std.file;
import std.stdio;
import std.utf;
import common;
import lox_debug;
import value;
import lox_vm;
import core.stdc.stdlib;
import lox_compiler;
import lox_object;

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
	auto func = compile(source);
	
	if (func == null) {
		return InterpretResult.COMPILE_ERROR;
	}

	auto funcValue = new Value(new Obj(func));
	vm.push(funcValue);
	vm.call(func, 0);
	
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
