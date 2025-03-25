namespace Trilang.Parsing.Nodes;

public interface ISyntaxNode
{
    void Accept(IVisitor visitor);
}