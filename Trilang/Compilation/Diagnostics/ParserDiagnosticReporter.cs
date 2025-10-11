using Trilang.Lexing;

namespace Trilang.Compilation.Diagnostics;

// TODO: better error messages
public class ParserDiagnosticReporter
{
    private readonly DiagnosticCollection diagnostics;
    private readonly SourceFile file;

    public ParserDiagnosticReporter(DiagnosticCollection diagnostics, SourceFile file)
    {
        this.diagnostics = diagnostics;
        this.file = file;
    }

    public void MissingToken(SourcePosition position, TokenKind expected)
        => diagnostics.Error(
            DiagnosticId.P0001MissingToken,
            new SourceLocation(file, position.ToSpan()),
            $"Expected '{expected.ToDisplayString()}'.");

    public void ExpectedParameter(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0002ExpectedParameter,
            new SourceLocation(file, span),
            "Expected a parameter.");

    public void ExpectedType(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0003ExpectedType,
            new SourceLocation(file, span),
            "Expected a type.");

    public void ExpectedStatement(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0004ExpectedStatement,
            new SourceLocation(file, span),
            "Expected a statement.");

    public void ExpectedTypeName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticId.P0005ExpectedTypeName,
            new SourceLocation(file, position.ToSpan()),
            "Expected a type name.");

    public void ExpectedInterface(SourcePosition position)
        => diagnostics.Error(
            DiagnosticId.P0006ExpectedInterface,
            new SourceLocation(file, position.ToSpan()),
            "Expected an interface.");

    public void ExpectedMethodName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticId.P0007ExpectedMethodName,
            new SourceLocation(file, position.ToSpan()),
            "Expected a method name.");

    public void ExpectedVariableName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticId.P0008ExpectedVariableName,
            new SourceLocation(file, position.ToSpan()),
            "Expected a variable name.");

    public void ExpectedExpression(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0009ExpectedExpression,
            new SourceLocation(file, span),
            "Expected an expression.");

    public void ExpectedDeclaration(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0010ExpectedDeclaration,
            new SourceLocation(file, span),
            "Expected a type or a function.");

    public void ExpectedDirectiveName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticId.P0011ExpectedDirectiveName,
            new SourceLocation(file, position.ToSpan()),
            "Expected a directive name.");

    public void ExpectedInterfaceParameters(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0012ExpectedInterfaceParameters,
            new SourceLocation(file, span),
            "Expected interface parameters.");

    public void ExpectedIdentifier(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0013ExpectedIdentifier,
            new SourceLocation(file, span),
            "Expected an identifier.");

    public void ExpectedTypeMember(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.P0014ExpectedTypeMember,
            new SourceLocation(file, span),
            "Expected a type member (a property, a method or a constructor).");
}