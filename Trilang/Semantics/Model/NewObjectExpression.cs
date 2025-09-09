using System.Diagnostics;
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class NewObjectExpression : IExpression
{
    public NewObjectExpression(IInlineType type, IReadOnlyList<IExpression> parameters)
    {
        Debug.Assert(type is Model.Type or GenericType);

        Type = type;
        Parameters = parameters;

        Type.Parent = this;

        foreach (var parameter in Parameters)
            parameter.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewObject(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewObject(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNewObject(this);

    public IExpression Clone()
        => new NewObjectExpression(Type.Clone(), Parameters.Select(x => x.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public IInlineType Type { get; }

    public IReadOnlyList<IExpression> Parameters { get; }

    public ConstructorMetadata? Metadata { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Metadata?.DeclaringType;
}