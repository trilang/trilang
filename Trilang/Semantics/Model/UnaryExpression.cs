using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class UnaryExpression : IExpression
{
    public UnaryExpression(SourceSpan? sourceSpan, UnaryExpressionKind kind, IExpression operand)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Operand = operand;
        Operand.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitUnaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitUnaryExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformUnaryExpression(this);

    public IExpression Clone()
        => new UnaryExpression(SourceSpan, Kind, Operand.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public UnaryExpressionKind Kind { get; }

    public IExpression Operand { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}