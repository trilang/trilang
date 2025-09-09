namespace Trilang.Parsing.Ast;

public interface ISyntaxNode
{
    void Accept(INodeVisitor visitor);

    TResult Transform<TResult>(INodeTransformer<TResult> transformer);
}