namespace Trilang.Compilation.Diagnostics;

public record Diagnostic(
    string Id,
    DiagnosticSeverity Severity,
    SourceLocation Location,
    string Message);