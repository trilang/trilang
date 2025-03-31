using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor<TContext>
{
    void Visit(BinaryExpressionNode node, TContext context);

    void Visit(BlockStatementNode node, TContext context);

    void Visit(CallExpressionNode node, TContext context);

    void Visit(ExpressionStatementNode node, TContext context);

    void Visit(FunctionParameterNode node, TContext context);

    void Visit(FunctionDeclarationNode node, TContext context);

    void Visit(IfStatementNode node, TContext context);

    void Visit(LiteralExpressionNode node, TContext context);

    void Visit(ReturnStatementNode node, TContext context);

    void Visit(SyntaxTree node, TContext context);

    void Visit(UnaryExpressionNode node, TContext context);

    void Visit(VariableExpressionNode node, TContext context);

    void Visit(VariableDeclarationStatementNode node, TContext context);
}