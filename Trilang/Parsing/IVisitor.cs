using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor
{
    void Visit(BinaryExpressionNode node);

    void Visit(BlockStatementNode node);

    void Visit(BreakNode node);

    void Visit(CallExpressionNode node);

    void Visit(ContinueNode node);

    void Visit(ExpressionStatementNode node);

    void Visit(FunctionParameterNode node);

    void Visit(FunctionDeclarationNode node);

    void Visit(IfStatementNode node);

    void Visit(LiteralExpressionNode node);

    void Visit(ReturnStatementNode node);

    void Visit(SyntaxTree node);

    void Visit(UnaryExpressionNode node);

    void Visit(VariableExpressionNode node);

    void Visit(VariableDeclarationStatementNode node);

    void Visit(WhileNode node);
}