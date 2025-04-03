using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolFinder : IVisitor<SymbolFinderContext>
{
    public void Visit(BinaryExpressionNode node, SymbolFinderContext context)
    {
        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    public void Visit(BlockStatementNode node, SymbolFinderContext context)
        => context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var statement in node.Statements)
                statement.Accept(this, c);
        });

    public void Visit(BreakNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(CallExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void Visit(ContinueNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(ExpressionStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(FunctionParameterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new VariableSymbol(node);
        if (!context.SymbolTable.TryAddVariable(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' variable is already defined.");

        node.Type.Accept(this, context);
    }

    public void Visit(FunctionDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new FunctionSymbol(node);
        if (!context.SymbolTable.TryAddFunction(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' function is already defined.");

        node.ReturnType.Accept(this, context);

        context.Scoped(c =>
        {
            c.DisableNextScope();

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body?.Accept(this, c);
        });
    }

    public void Visit(IfStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void Visit(LiteralExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(ReturnStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(SyntaxTree node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var function in node.Functions)
            function.Accept(this, context);
    }

    public void Visit(TypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        if (node.IsArray)
            context.SymbolTable.TryAddType(new TypeSymbol($"{node.Name}[]", true, null));
    }

    public void Visit(UnaryExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Operand.Accept(this, context);
    }

    public void Visit(VariableExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(VariableDeclarationStatementNode node, SymbolFinderContext context)
    {
        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        var symbol = new VariableSymbol(node);
        if (!context.SymbolTable.TryAddVariable(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' variable is already defined.");

        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(WhileNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }
}