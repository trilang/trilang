using Trilang.Lexing;

namespace Trilang.Compilation.Diagnostics;

// TODO: better error messages
internal readonly record struct ParserDiagnosticReporter(DiagnosticCollection Diagnostics, SourceFile File)
{
    public void MissingToken(SourcePosition position, TokenKind expected)
        => Diagnostics.Error(
            DiagnosticId.P0001MissingToken,
            new SourceLocation(File, position.ToSpan()),
            $"Expected '{expected.ToDisplayString()}'.");

    public void ExpectedParameter(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0002ExpectedParameter,
            new SourceLocation(File, span),
            "Expected a parameter.");

    public void ExpectedType(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0003ExpectedType,
            new SourceLocation(File, span),
            "Expected a type.");

    public void ExpectedStatement(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0004ExpectedStatement,
            new SourceLocation(File, span),
            "Expected a statement.");

    public void ExpectedTypeName(SourcePosition position)
        => Diagnostics.Error(
            DiagnosticId.P0005ExpectedTypeName,
            new SourceLocation(File, position.ToSpan()),
            "Expected a type name.");

    public void ExpectedInterface(SourcePosition position)
        => Diagnostics.Error(
            DiagnosticId.P0006ExpectedInterface,
            new SourceLocation(File, position.ToSpan()),
            "Expected an interface.");

    public void ExpectedMethodName(SourcePosition position)
        => Diagnostics.Error(
            DiagnosticId.P0007ExpectedMethodName,
            new SourceLocation(File, position.ToSpan()),
            "Expected a method name.");

    public void ExpectedVariableName(SourcePosition position)
        => Diagnostics.Error(
            DiagnosticId.P0008ExpectedVariableName,
            new SourceLocation(File, position.ToSpan()),
            "Expected a variable name.");

    public void ExpectedExpression(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0009ExpectedExpression,
            new SourceLocation(File, span),
            "Expected an expression.");

    public void ExpectedDeclaration(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0010ExpectedDeclaration,
            new SourceLocation(File, span),
            "Expected a type or a function.");

    public void ExpectedDirectiveName(SourcePosition position)
        => Diagnostics.Error(
            DiagnosticId.P0011ExpectedDirectiveName,
            new SourceLocation(File, position.ToSpan()),
            "Expected a directive name.");

    public void ExpectedInterfaceParameters(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0012ExpectedInterfaceParameters,
            new SourceLocation(File, span),
            "Expected interface parameters.");

    public void ExpectedIdentifier(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0013ExpectedIdentifier,
            new SourceLocation(File, span),
            "Expected an identifier.");

    public void ExpectedTypeMember(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0014ExpectedTypeMember,
            new SourceLocation(File, span),
            "Expected a type member (a property, a method or a constructor).");

    public void ExpectedNamespacePart(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0015ExpectedNamespacePart,
            new SourceLocation(File, span),
            "Expected a namespace part.");

    public void ExpectedNamespace(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.P0016ExpectedNamespace,
            new SourceLocation(File, span),
            "Expected a namespace.");
}