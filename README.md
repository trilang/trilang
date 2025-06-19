# Trilang

> [!NOTE]
> A programming language implementation built with .NET and C#. Later, when the basic compiler features and standard library are ready, it will be rewritten in Tri.

<img src="https://github.com/trilang/trilang/blob/master/Tri.png" width="128">

## Overview

TODO

## Project Structure

- **`Tri/`** - Main executable application
- **`Trilang/`** - Core language implementation library
    - `Lexing/` - Lexical analysis components
    - `Parsing/` - Parser implementation
    - `Semantics/` - Semantic analysis
    - `Symbols/` - Symbol table management
    - `Compilation/` - Compilation pipeline
    - `IntermediateRepresentation/` - IR generation and manipulation
    - `Lower/` - Lowering transformations
    - `OutputFormats/` - Code generation backends
    - `Metadata/` - Metadata handling
- **`Tri.Tests/`** - Unit tests using NUnit framework
- **`example.tri`** - Example Tri language source file

## Requirements

- .NET 9.0 or later

## Building

To build the project:

```bash
dotnet build
```

## Running

To run the Tri compiler:

```bash
dotnet run --project Tri
```

To help with command-line options:

```bash
dotnet run --project Tri -- --help
```

To run tests:

```bash
dotnet test
```

## Example Usage

See for a sample Tri language program demonstrating the syntax and features: [example.tri](https://github.com/trilang/trilang/blob/master/example.tri).

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License
See the [LICENSE](https://github.com/trilang/trilang/blob/master/LICENSE) file for details.

## Code Owners
See the [CODEOWNERS](https://github.com/trilang/trilang/blob/master/CODEOWNERS) file for maintainer information.
