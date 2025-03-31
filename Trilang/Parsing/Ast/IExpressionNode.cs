using Trilang.Metadata;

namespace Trilang.Parsing.Ast;

public interface IExpressionNode : ISyntaxNode
{
    TypeMetadata? ReturnTypeMetadata { get; }
}