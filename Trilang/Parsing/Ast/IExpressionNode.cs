using Trilang.Metadata;

namespace Trilang.Parsing.Ast;

public interface IExpressionNode : ISyntaxNode
{
    IMetadata? ReturnTypeMetadata { get; }
}