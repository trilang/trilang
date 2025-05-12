using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

internal class SymbolFinder : IVisitor<SymbolFinderContext>
{
    public void Visit(ArrayAccessExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

    public void Visit(ArrayTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.SymbolTable.TryAddType(TypeSymbol.Array(node));

        node.ElementType.Accept(this, context);
    }

    public void Visit(BinaryExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

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

            c.SymbolTable.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void Visit(ContinueNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(DiscriminatedUnionNode node, SymbolFinderContext context)
    {
        var symbol = TypeSymbol.DiscriminatedUnion(node);
        context.SymbolTable.TryAddType(symbol);

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var type in node.Types)
                type.Accept(this, c);
        });
    }

    public void Visit(ExpressionStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void Visit(FunctionDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' function is already defined.");

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
        var symbol = TypeSymbol.FunctionType(node);
        context.SymbolTable.TryAddType(symbol);

        node.SymbolTable = context.SymbolTable;

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void Visit(GenericTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);

        var symbol = TypeSymbol.GenericType(node);
        node.SymbolTable.TryAddType(symbol);
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
        var symbol = TypeSymbol.Interface(node);
        context.SymbolTable.TryAddType(symbol);

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var property in node.Properties)
                property.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void Visit(InterfacePropertyNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");
    }

    public void Visit(InterfaceMethodNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        context.Scoped(c =>
        {
            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.ReturnType.Accept(this, c);
        });
    }

    public void Visit(LiteralExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(MemberAccessExpressionNode node, SymbolFinderContext context)
    {
        node.Member?.Accept(this, context);

        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(MethodDeclarationNode node, SymbolFinderContext context)
    {
        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        node.SymbolTable = context.SymbolTable;

        node.ReturnType.Accept(this, context);

        context.Scoped(c =>
        {
            c.DisableNextScope();

            c.SymbolTable.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void Visit(NewArrayExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);
    }

    public void Visit(NewObjectExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void Visit(NullExpressionNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void Visit(ReturnStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression?.Accept(this, context);
    }

    public void Visit(ParameterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' parameter is already defined.");

        node.Type.Accept(this, context);
    }

    public void Visit(PropertyDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void Visit(PropertyGetterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
            if (!c.SymbolTable.TryAddId(fieldSymbol))
                throw new SemanticAnalysisException();

            node.Body?.Accept(this, c);
        });
    }

    public void Visit(PropertySetterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
            if (!c.SymbolTable.TryAddId(fieldSymbol))
                throw new SemanticAnalysisException();

            var valueSymbol = new IdSymbol(MemberAccessExpressionNode.Value, node.Parent);
            if (!c.SymbolTable.TryAddId(valueSymbol))
                throw new SemanticAnalysisException();

            node.Body?.Accept(this, c);
        });
    }

    public void Visit(SyntaxTree node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var function in node.Declarations)
            function.Accept(this, context);
    }

    public void Visit(TupleExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var expression in node.Expressions)
            expression.Accept(this, context);
    }

    public void Visit(TupleTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var type in node.Types)
            type.Accept(this, context);

        var symbol = TypeSymbol.Tuple(node);
        context.SymbolTable.TryAddType(symbol);
    }

    public void Visit(TypeAliasDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        if (!context.SymbolTable.TryAddType(TypeSymbol.Alias(node)))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        node.Type.Accept(this, context);
    }

    public void Visit(TypeDeclarationNode node, SymbolFinderContext context)
    {
        var symbol = node.GenericArguments.Count > 0
            ? TypeSymbol.OpenGenericType(node)
            : TypeSymbol.Type(node);
        if (!context.SymbolTable.TryAddType(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var genericArgument in node.GenericArguments)
                genericArgument.Accept(this, c);

            foreach (var @interface in node.Interfaces)
                @interface.Accept(this, c);

            foreach (var property in node.Properties)
                property.Accept(this, c);

            foreach (var constructor in node.Constructors)
                constructor.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void Visit(TypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
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

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' variable is already defined.");

        node.SymbolTable = context.SymbolTable;
    }

    public void Visit(WhileNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }
}