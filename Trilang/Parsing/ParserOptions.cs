using Trilang.Compilation.Diagnostics;

namespace Trilang.Parsing;

public record ParserOptions(SourceFile SourceFile, ParserDiagnosticReporter Diagnostics);