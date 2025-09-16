namespace Trilang.Semantics.Model;

public class SemanticTree : ISemanticNode
{
    private readonly List<IDeclaration> declarations;

    public SemanticTree(SourceSpan? sourceSpan, IReadOnlyList<IDeclaration> declarations)
    {
        SourceSpan = sourceSpan;
        this.declarations = [..declarations];

        foreach (var function in declarations)
            function.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTree(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTree(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTree(this);

    public void Insert(int i, IDeclaration declaration)
    {
        declaration.Parent = this;
        declarations.Insert(i, declaration);
    }

    public void Remove(IDeclaration declaration)
        => declarations.Remove(declaration);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IReadOnlyList<IDeclaration> Declarations
        => declarations;
}