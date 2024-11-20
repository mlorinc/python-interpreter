# IPP Interpreter
This project implements an **interpreter** for a custom instruction set defined in XML. The interpreter reads, validates, and directly executes instructions dynamically. It supports advanced features such as labels, arithmetic operations, logical expressions, and stack-based memory management.

## Features
- **Dynamic Instruction Set**: 
  - Define and register custom instructions dynamically using Python functions.
- **Instruction Compilation**: 
  - Validates and compiles instructions before execution.
- **Runtime Execution**: 
  - Executes a sequence of instructions with support for jumps, calls, and stack-based operations.
- **Memory Management**:
  - Supports global, local, and temporary frames with variable declarations and assignments.
- **Error Handling**:
  - Comprehensive error handling for syntax, runtime, and semantic issues.

## Project Structure
### Classes

#### 1. **Instruction**
- Represents an individual instruction in the interpreter.
- Contains:
  - `opcode`: The operation code (e.g., `ADD`, `JUMP`).
  - `args`: Arguments for the instruction.
  - `order`: The execution order of the instruction.
- **Static Methods**:
  - `register`: Registers a Python function as an instruction implementation.

#### 2. **Argument**
- Represents arguments for instructions.
- Types include variables, literals, symbols, and labels.
- **Key Methods**:
  - `parse`: Parses raw arguments into usable data.
  - `fromStack`: Creates an argument object from stack values.

#### 3. **Memory**
- Manages:
  - Global (`GF`), Local (`LF`), and Temporary (`TF`) frames.
  - A call stack and a data stack for runtime operations.
- Provides methods for:
  - Declaring, assigning, and retrieving variables.
  - Stack operations (push/pop).

#### 4. **IPPInterpreter**
- The main interpreter class.
- Handles:
  - Compiling instructions.
  - Optimizing labels for faster lookups.
  - Executing instructions with proper flow control (jumps, calls, returns).
  - Maintaining a call stack.
