using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public interface IExpression : ISemanticNode
{
    ITypeMetadata? ReturnTypeMetadata { get; }

    IExpression Clone();
}