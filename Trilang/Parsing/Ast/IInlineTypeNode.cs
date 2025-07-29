using Trilang.Metadata;

namespace Trilang.Parsing.Ast;

public interface IInlineTypeNode : ISyntaxNode
{
    string Name { get; }

    ITypeMetadata? Metadata { get; }

    IInlineTypeNode Clone();
}