namespace Trilang.Compilation.Diagnostics;

public record Diagnostic(
    string Id,
    DiagnosticSeverity Severity,
    SourceFile File,
    SourceSpan SourceSpan,
    string Message
);