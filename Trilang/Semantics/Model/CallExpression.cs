using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class CallExpression : IExpression
{
    public CallExpression(IExpression member, IReadOnlyList<IExpression> parameters)
    {
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
        => new CallExpression(Member.Clone(), Parameters.Select(x => x.Clone()).ToArray());

    public ISemanticNode? Parent { get; set; }

    public IExpression Member { get; }

    public IReadOnlyList<IExpression> Parameters { get; }

    public FunctionTypeMetadata? Metadata
        => Member.ReturnTypeMetadata as FunctionTypeMetadata;

    public ITypeMetadata? ReturnTypeMetadata
        => Metadata?.ReturnType;
}