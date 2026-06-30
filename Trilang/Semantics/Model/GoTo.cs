using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class GoTo : IStatement
{
    public GoTo(string label)
        => Label = label;

    public void Accept(IVisitor visitor)
        => visitor.VisitGoTo(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGoTo(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformGoTo(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan => null;

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string Label { get; }
}