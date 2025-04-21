using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolFinder : IVisitor<SymbolFinderContext>
{
    public void Visit(ArrayAccessExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

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

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void Visit(ConstructorDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void Visit(ContinueNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(ExpressionStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(FieldDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

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

    public void Visit(FunctionTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void Visit(IfStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void Visit(InterfaceNode node, SymbolFinderContext context)
    {
        // TODO: define in inner scope?
        node.SymbolTable = context.SymbolTable;

        var symbol = TypeSymbol.Interface(node);
        context.SymbolTable.TryAddType(symbol);

        context.Scoped(c =>
        {
            foreach (var field in node.Fields)
                field.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void Visit(InterfaceFieldNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);
    }

    public void Visit(InterfaceMethodNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void Visit(LiteralExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(MemberAccessExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(MethodDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.ReturnType.Accept(this, context);

        context.Scoped(c =>
        {
            c.DisableNextScope();

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void Visit(ReturnStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression?.Accept(this, context);
    }

    public void Visit(ParameterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new VariableSymbol(node);
        if (!context.SymbolTable.TryAddVariable(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' variable is already defined.");

        node.Type.Accept(this, context);
    }

    public void Visit(SyntaxTree node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var function in node.Declarations)
            function.Accept(this, context);
    }

    public void Visit(TypeAliasDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        if (node.Type is FunctionTypeNode functionType)
            context.SymbolTable.TryAddType(TypeSymbol.FunctionType(functionType));

        if (!context.SymbolTable.TryAddType(TypeSymbol.Alias(node)))
            throw new SymbolTableBuilderException($"The '{node.Name}' type is already defined.");

        node.Type.Accept(this, context);
    }

    public void Visit(TypeDeclarationNode node, SymbolFinderContext context)
    {
        // TODO: define in inner scope?
        node.SymbolTable = context.SymbolTable;

        var symbol = TypeSymbol.Type(node);
        if (!context.SymbolTable.TryAddType(symbol))
            throw new SymbolTableBuilderException($"The '{node.Name}' type is already defined.");

        context.Scoped(c =>
        {
            foreach (var field in node.Fields)
                field.Accept(this, c);

            foreach (var constructor in node.Constructors)
                constructor.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void Visit(TypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        if (node.IsArray)
            context.SymbolTable.TryAddType(TypeSymbol.Array(node.Name));
    }

    public void Visit(UnaryExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Operand.Accept(this, context);
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