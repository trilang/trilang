using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class CallExpression : IAccessExpression
{
    public CallExpression(SourceSpan? sourceSpan, IAccessExpression member, IReadOnlyList<IExpression> parameters)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Parameters = parameters;

        Member.Parent = this;
        foreach (var parameter in parameters)
            parameter.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitCall(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitCall(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformCall(this);

    public IExpression Clone()
        => new CallExpression(SourceSpan, (IAccessExpression)Member.Clone(), Parameters.Select(x => x.Clone()).ToArray())
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public IAccessExpression Member { get; }

    public IReadOnlyList<IExpression> Parameters { get; }

    public FunctionTypeMetadata? Metadata
        => Member.ReturnTypeMetadata as FunctionTypeMetadata;

    public ITypeMetadata? ReturnTypeMetadata
        => Metadata?.ReturnType;

    public IMetadata? Reference
    {
        get => Member.Reference;
        set => Member.Reference = value;
    }
}