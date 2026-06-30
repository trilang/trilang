using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class Continue : IStatement
{
    public Continue(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitContinue(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitContinue(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformContinue(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public While? LoopNode { get; set; }
}