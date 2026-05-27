using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public interface IAccessExpression : IExpression
{
    IMetadata? Reference { get; set; }
}