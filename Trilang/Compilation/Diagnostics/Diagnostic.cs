namespace Trilang.Compilation.Diagnostics;

public record Diagnostic(
    DiagnosticId Id,
    DiagnosticSeverity Severity,
    SourceLocation Location,
    string Message);