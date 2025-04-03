using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor<TContext, TResult> : IVisitor<TContext>
    where TContext : VisitorContext<TResult>
{
    public void Visit(BinaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BinaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BinaryExpressionNode node, TContext context)
    {
    }

    public void Visit(BlockStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BlockStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BlockStatementNode node, TContext context)
    {
    }

    public void Visit(BreakNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BreakNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BreakNode node, TContext context)
    {
    }

    public void Visit(CallExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(CallExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(CallExpressionNode node, TContext context)
    {
    }

    public void Visit(ContinueNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ContinueNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ContinueNode node, TContext context)
    {
    }

    public void Visit(ExpressionStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Expression.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ExpressionStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ExpressionStatementNode node, TContext context)
    {
    }

    public void Visit(FunctionParameterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(FunctionParameterNode node, TContext context)
    {
    }

    protected virtual void VisitExit(FunctionParameterNode node, TContext context)
    {
    }

    public void Visit(FunctionDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(FunctionDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(FunctionDeclarationNode node, TContext context)
    {
    }

    public void Visit(IfStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(IfStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(IfStatementNode node, TContext context)
    {
    }

    public void Visit(LiteralExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(LiteralExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(LiteralExpressionNode node, TContext context)
    {
    }

    public void Visit(ReturnStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Expression.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ReturnStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ReturnStatementNode node, TContext context)
    {
    }

    public void Visit(SyntaxTree node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var function in node.Functions)
            function.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(SyntaxTree node, TContext context)
    {
    }

    protected virtual void VisitExit(SyntaxTree node, TContext context)
    {
    }

    public void Visit(TypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TypeNode node, TContext context)
    {
    }

    public void Visit(UnaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Operand.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(UnaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(UnaryExpressionNode node, TContext context)
    {
    }

    public void Visit(VariableExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(VariableExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(VariableExpressionNode node, TContext context)
    {
    }

    public void Visit(VariableDeclarationStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(VariableDeclarationStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(VariableDeclarationStatementNode node, TContext context)
    {
    }

    public void Visit(WhileNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(WhileNode node, TContext context)
    {
    }

    protected virtual void VisitExit(WhileNode node, TContext context)
    {
    }
}