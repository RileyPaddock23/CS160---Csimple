# Csimple Compiler

A compiler front-end for **Csimple**, a simple statically-typed programming language, developed as part of CS 160 (Compilers).

## Overview

The Csimple compiler translates source code written in the Csimple language into an **Abstract Syntax Tree (AST)**, performs **semantic analysis** (type checking), and outputs the AST as a [GraphViz](https://graphviz.org/) DOT graph for visualization.

### Compilation Pipeline

```
Source Code (.csimple)
        │
        ▼
  Lexical Analysis   (lexer.l  → Flex)
        │
        ▼
  Syntax Analysis    (parser.ypp → Bison)
        │
        ▼
  AST Construction   (ast.cdef → astbuilder.gawk → ast.cpp/ast.hpp)
        │
        ▼
  Semantic Analysis  (typecheck.cpp — Visitor pattern)
        │
        ▼
  DOT Graph Output   (ast2dot.cpp)
```

## Prerequisites

| Tool   | Purpose                              |
|--------|--------------------------------------|
| `g++`  | C++11 compiler                       |
| `flex` | Lexical analyzer generator           |
| `bison`| Parser generator (LALR)              |
| `gawk` | AWK interpreter for AST code generation |
| `dot`  | GraphViz (optional, for rendering)   |

On Debian/Ubuntu:

```bash
sudo apt-get install g++ flex bison gawk graphviz
```

## Building

```bash
# Build the compiler
make

# Regenerate AST source files from ast.cdef (only needed if ast.cdef changes)
make ast

# Remove build artifacts (keeps generated ast.cpp/ast.hpp)
make clean

# Remove all generated files including ast.cpp/ast.hpp
make veryclean
```

The build produces a single executable: `csimple`.

## Usage

```bash
./csimple < program.csimple
```

The compiler reads Csimple source from **stdin** and writes the AST as a GraphViz DOT graph to **stdout**. Errors are reported to **stderr**.

### Visualizing the AST

Pipe the output directly into GraphViz to render a PNG:

```bash
./csimple < program.csimple | dot -Tpng -o ast.png
```

### Example

Given a file `hello.csimple`:

```
procedure Main() return integer {
    var x : integer;
    x = 42;
    return x;
}
```

Run:

```bash
./csimple < hello.csimple | dot -Tpng -o hello.png
```

## Language Reference

See [docs/language-reference.md](docs/language-reference.md) for the complete Csimple language syntax and features.

## Architecture

See [docs/architecture.md](docs/architecture.md) for a detailed walkthrough of the compiler's internal architecture, including the AST code-generation pipeline, symbol table, and type checker.

## Error Codes

The compiler exits with a non-zero status code when a semantic error is detected. See [docs/error-codes.md](docs/error-codes.md) for a full list of error codes and their meanings.

## Project Structure

```
.
├── Makefile            # Build configuration
├── ast.cdef            # AST node definitions (DSL input to astbuilder.gawk)
├── astbuilder.gawk     # AWK script that generates ast.cpp and ast.hpp
├── ast.cpp             # Auto-generated AST node implementations
├── ast.hpp             # Auto-generated AST node declarations
├── ast2dot.cpp         # Visitor: converts AST to GraphViz DOT format
├── attribute.hpp       # Attribute/type annotation structs for AST nodes
├── lexer.l             # Flex lexical grammar
├── main.cpp            # Entry point
├── parser.ypp          # Bison LALR grammar
├── primitive.cpp       # Integer/string literal value wrappers
├── primitive.hpp
├── symtab.cpp          # Symbol table implementation
├── symtab.hpp          # Symbol table declarations
├── typecheck.cpp       # Visitor: semantic analysis / type checking
└── docs/
    ├── architecture.md
    ├── error-codes.md
    └── language-reference.md
```

## License

This project was developed for educational purposes as part of CS 160.
