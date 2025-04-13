using Trilang.Metadata;

namespace Trilang.Parsing.Ast;

public interface IExpressionNode : ISyntaxNode
{
    ITypeMetadata? ReturnTypeMetadata { get; }
}