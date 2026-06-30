using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

// TODO: replace by pattern matching
public class IsExpression : IExpression
{
    private readonly BuiltInTypes builtInTypes;

    public IsExpression(
        SourceSpan? sourceSpan,
        IExpression expression,
        IInlineType type,
        BuiltInTypes builtInTypes)
    {
        this.builtInTypes = builtInTypes;
        SourceSpan = sourceSpan;
        Expression = expression;
        Type = type;

        Expression.Parent = this;
        Type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitIsExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitIsExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformIsExpression(this);

    public IExpression Clone()
        => new IsExpression(SourceSpan, Expression.Clone(), Type.Clone(), builtInTypes)
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public IExpression Expression { get; }

    public IInlineType Type { get; }

    public ITypeMetadata ReturnTypeMetadata
        => builtInTypes.Bool;
}