using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor : IVisitor
{
    public void Visit(ArrayAccessExpressionNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(ArrayAccessExpressionNode node)
    {
    }

    protected virtual void VisitExit(ArrayAccessExpressionNode node)
    {
    }

    public void Visit(BinaryExpressionNode node)
    {
        VisitEnter(node);

        node.Left.Accept(this);
        node.Right.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(BinaryExpressionNode node)
    {
    }

    protected virtual void VisitExit(BinaryExpressionNode node)
    {
    }

    public void Visit(BlockStatementNode node)
    {
        VisitEnter(node);

        foreach (var statement in node.Statements)
            statement.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(BlockStatementNode node)
    {
    }

    protected virtual void VisitExit(BlockStatementNode node)
    {
    }

    public void Visit(BreakNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(BreakNode node)
    {
    }

    protected virtual void VisitExit(BreakNode node)
    {
    }

    public void Visit(CallExpressionNode node)
    {
        VisitEnter(node);

        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(CallExpressionNode node)
    {
    }

    protected virtual void VisitExit(CallExpressionNode node)
    {
    }

    public void Visit(ContinueNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(ContinueNode node)
    {
    }

    protected virtual void VisitExit(ContinueNode node)
    {
    }

    public void Visit(ExpressionStatementNode node)
    {
        VisitEnter(node);

        node.Expression.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ExpressionStatementNode node)
    {
    }

    protected virtual void VisitExit(ExpressionStatementNode node)
    {
    }

    public void Visit(FunctionParameterNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(FunctionParameterNode node)
    {
    }

    protected virtual void VisitExit(FunctionParameterNode node)
    {
    }

    public void Visit(FunctionDeclarationNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);
        node.Body?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(FunctionDeclarationNode node)
    {
    }

    protected virtual void VisitExit(FunctionDeclarationNode node)
    {
    }

    public void Visit(IfStatementNode node)
    {
        VisitEnter(node);

        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(IfStatementNode node)
    {
    }

    protected virtual void VisitExit(IfStatementNode node)
    {
    }

    public void Visit(LiteralExpressionNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(LiteralExpressionNode node)
    {
    }

    protected virtual void VisitExit(LiteralExpressionNode node)
    {
    }

    public void Visit(MemberAccessExpressionNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(MemberAccessExpressionNode node)
    {
    }

    protected virtual void VisitExit(MemberAccessExpressionNode node)
    {
    }

    public void Visit(ReturnStatementNode node)
    {
        VisitEnter(node);

        node.Expression.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ReturnStatementNode node)
    {
    }

    protected virtual void VisitExit(ReturnStatementNode node)
    {
    }

    public void Visit(SyntaxTree node)
    {
        VisitEnter(node);

        foreach (var function in node.Functions)
            function.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(SyntaxTree node)
    {
    }

    protected virtual void VisitExit(SyntaxTree node)
    {
    }

    public void Visit(TypeNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(TypeNode node)
    {
    }

    protected virtual void VisitExit(TypeNode node)
    {
    }

    public void Visit(UnaryExpressionNode node)
    {
        VisitEnter(node);

        node.Operand.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(UnaryExpressionNode node)
    {
    }

    protected virtual void VisitExit(UnaryExpressionNode node)
    {
    }

    public void Visit(VariableDeclarationStatementNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(VariableDeclarationStatementNode node)
    {
    }

    protected virtual void VisitExit(VariableDeclarationStatementNode node)
    {
    }

    public void Visit(WhileNode node)
    {
        VisitEnter(node);

        node.Condition.Accept(this);
        node.Body.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(WhileNode node)
    {
    }

    protected virtual void VisitExit(WhileNode node)
    {
    }
}