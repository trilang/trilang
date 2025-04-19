using Trilang.Metadata;

namespace Trilang.Parsing.Ast;

public interface IInlineTypeNode : ISyntaxNode
{
    string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}