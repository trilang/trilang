using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class Label : IStatement
{
    public Label(string name)
        => Name = name;

    public void Accept(IVisitor visitor)
        => visitor.VisitLabel(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitLabel(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformLabel(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan => null;

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string Name { get; }
}