using Trilang.Lexing;

namespace Trilang.Compilation.Diagnostics;

// TODO: better error messages
public class ParserDiagnosticCollection
{
    private readonly DiagnosticCollection diagnostics;

    public ParserDiagnosticCollection(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics;

    public void MissingToken(SourcePosition position, TokenKind expected)
        => diagnostics.Error(
            DiagnosticIds.P0001_MissingToken,
            position.ToSpan(),
            $"Expected '{expected.ToDisplayString()}'.");

    public void ExpectedParameter(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0002_ExpectedParameter,
            span,
            "Expected a parameter.");

    public void ExpectedType(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0003_ExpectedType,
            span,
            "Expected a type.");

    public void ExpectedStatement(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0004_ExpectedStatement,
            span,
            "Expected a statement.");

    public void ExpectedTypeName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticIds.P0005_ExpectedTypeName,
            position.ToSpan(),
            "Expected a type name.");

    public void ExpectedInterface(SourcePosition position)
        => diagnostics.Error(
            DiagnosticIds.P0006_ExpectedInterface,
            position.ToSpan(),
            "Expected an interface.");

    public void ExpectedMethodName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticIds.P0007_ExpectedMethodName,
            position.ToSpan(),
            "Expected a method name.");

    public void ExpectedVariableName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticIds.P0008_ExpectedVariableName,
            position.ToSpan(),
            "Expected a variable name.");

    public void ExpectedExpression(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0009_ExpectedExpression,
            span,
            "Expected an expression.");

    public void ExpectedDeclaration(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0010_ExpectedDeclaration,
            span,
            "Expected a type or a function.");

    public void ExpectedDirectiveName(SourcePosition position)
        => diagnostics.Error(
            DiagnosticIds.P0011_ExpectedDirectiveName,
            position.ToSpan(),
            "Expected a directive name.");

    public void ExpectedInterfaceParameters(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0012_ExpectedInterfaceParameters,
            span,
            "Expected interface parameters.");

    public void ExpectedIdentifier(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0013_ExpectedIdentifier,
            span,
            "Expected an identifier.");

    public void ExpectedTypeMember(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.P0014_ExpectedTypeMember,
            span,
            "Expected a type member (a property, a method or a constructor).");
}