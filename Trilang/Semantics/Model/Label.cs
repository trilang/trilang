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

    public string Name { get; }
}