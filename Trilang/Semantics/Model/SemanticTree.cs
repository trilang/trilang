namespace Trilang.Semantics.Model;

public class SemanticTree : ISemanticNode
{
    private readonly List<IDeclaration> declarations;

    public SemanticTree(
        SourceFile sourceFile,
        SourceSpan? sourceSpan,
        Namespace? namespaceNode,
        IReadOnlyList<Use> useNodes,
        IReadOnlyList<IDeclaration> declarations)
    {
        SourceFile = sourceFile;
        SourceSpan = sourceSpan;
        Namespace = namespaceNode;
        UseNodes = useNodes;
        this.declarations = [.. declarations];

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

    public SourceFile SourceFile { get; }

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public Namespace? Namespace { get; }

    public IReadOnlyList<Use> UseNodes { get; }

    public IReadOnlyList<IDeclaration> Declarations
        => declarations;
}