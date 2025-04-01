using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolTableBuilder : IVisitor<SymbolTableBuilderContext>
{
    public void Visit(BinaryExpressionNode node, SymbolTableBuilderContext context)
    {
        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    public void Visit(BlockStatementNode node, SymbolTableBuilderContext context)
        => context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var statement in node.Statements)
                statement.Accept(this, c);
        });

    public void Visit(BreakNode node, SymbolTableBuilderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(CallExpressionNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void Visit(ContinueNode node, SymbolTableBuilderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(ExpressionStatementNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(FunctionParameterNode node, SymbolTableBuilderContext context)
    {
        var symbol = new VariableSymbol(node);
        if (!context.SymbolTable.TryAddVariable(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' variable is already defined.");

        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(FunctionDeclarationNode node, SymbolTableBuilderContext context)
    {
        var symbol = new FunctionSymbol(node);
        if (!context.SymbolTable.TryAddFunction(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' function is already defined.");

        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body?.Accept(this, c);
        });
    }

    public void Visit(IfStatementNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void Visit(LiteralExpressionNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(ReturnStatementNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(SyntaxTree node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var function in node.Functions)
            function.Accept(this, context);
    }

    public void Visit(UnaryExpressionNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Operand.Accept(this, context);
    }

    public void Visit(VariableExpressionNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(VariableDeclarationStatementNode node, SymbolTableBuilderContext context)
    {
        var symbol = new VariableSymbol(node);
        if (!context.SymbolTable.TryAddVariable(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' variable is already defined.");

        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(WhileNode node, SymbolTableBuilderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }
}