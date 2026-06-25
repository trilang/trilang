using System.Numerics;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class LiteralExpressionNode : IExpressionNode
{
    public LiteralExpressionNode(SourceSpan sourceSpan, LiteralExpressionKind kind, object value)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Value = value;
    }

    private static LiteralExpressionNode Number<T>(
        ParserDiagnosticReporter diagnostics,
        Token token,
        LiteralExpressionKind kind)
        where T : INumber<T>
    {
        if (!T.TryParse(token.Value?.ToString(), null, out var number))
        {
            number = T.Zero;
            diagnostics.InvalidNumber(token.SourceSpan);
        }

        return new LiteralExpressionNode(token.SourceSpan, kind, number);
    }

    internal static LiteralExpressionNode I8(ParserDiagnosticReporter diagnostics, Token token)
        => Number<sbyte>(diagnostics, token, LiteralExpressionKind.I8);

    internal static LiteralExpressionNode I16(ParserDiagnosticReporter diagnostics, Token token)
        => Number<short>(diagnostics, token, LiteralExpressionKind.I16);

    internal static LiteralExpressionNode I32(ParserDiagnosticReporter diagnostics, Token token)
        => Number<int>(diagnostics, token, LiteralExpressionKind.I32);

    internal static LiteralExpressionNode I64(ParserDiagnosticReporter diagnostics, Token token)
        => Number<long>(diagnostics, token, LiteralExpressionKind.I64);

    internal static LiteralExpressionNode U8(ParserDiagnosticReporter diagnostics, Token token)
        => Number<byte>(diagnostics, token, LiteralExpressionKind.U8);

    internal static LiteralExpressionNode U16(ParserDiagnosticReporter diagnostics, Token token)
        => Number<ushort>(diagnostics, token, LiteralExpressionKind.U16);

    internal static LiteralExpressionNode U32(ParserDiagnosticReporter diagnostics, Token token)
        => Number<uint>(diagnostics, token, LiteralExpressionKind.U32);

    internal static LiteralExpressionNode U64(ParserDiagnosticReporter diagnostics, Token token)
        => Number<ulong>(diagnostics, token, LiteralExpressionKind.U64);

    internal static LiteralExpressionNode F32(ParserDiagnosticReporter diagnostics, Token token)
        => Number<float>(diagnostics, token, LiteralExpressionKind.F32);

    internal static LiteralExpressionNode F64(ParserDiagnosticReporter diagnostics, Token token)
        => Number<double>(diagnostics, token, LiteralExpressionKind.F64);

    internal static LiteralExpressionNode True(SourceSpan sourceSpan)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Boolean, true);

    internal static LiteralExpressionNode False(SourceSpan sourceSpan)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Boolean, false);

    internal static LiteralExpressionNode String(SourceSpan sourceSpan, string str)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.String, str);

    internal static LiteralExpressionNode Char(SourceSpan sourceSpan, string c)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Char, c);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitLiteral(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformLiteral(this);

    public SourceSpan SourceSpan { get; }

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }
}