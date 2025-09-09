namespace Trilang.Semantics.Model;

public class IfDirective : IDeclaration, IStatement
{
    public IfDirective(string directiveName, IReadOnlyList<ISemanticNode> then, IReadOnlyList<ISemanticNode> @else)
    {
        DirectiveName = directiveName;
        Then = then;
        Else = @else;

        foreach (var node in Then)
            node.Parent = this;

        foreach (var node in Else)
            node.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitIfDirective(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitIfDirective(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformIfDirective(this);

    public ISemanticNode? Parent { get; set; }

    public string DirectiveName { get; }

    // IDeclarationNode | IStatementNode
    public IReadOnlyList<ISemanticNode> Then { get; }

    public IReadOnlyList<ISemanticNode> Else { get; }
}