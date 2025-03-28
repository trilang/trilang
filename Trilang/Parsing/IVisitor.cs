using Trilang.Parsing.Nodes;

namespace Trilang.Parsing;

public interface IVisitor
{
    void Visit(BinaryExpressionNode node);

    void Visit(BlockStatementNode node);

    void Visit(CallExpressionNode node);

    void Visit(ExpressionStatementNode node);

    void Visit(FunctionParameterNode node);

    void Visit(FunctionDeclarationNode node);

    void Visit(IfStatementNode node);

    void Visit(LiteralExpressionNode node);

    void Visit(ReturnStatementNode node);

    void Visit(SyntaxTree node);

    void Visit(UnaryExpressionNode node);

    void Visit(VariableExpressionNode node);

    void Visit(VariableDeclarationNode node);
}