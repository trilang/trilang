using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class TypeChecker : IVisitor
{
    private readonly TypeMetadataStore typeStore;

    public TypeChecker()
        => typeStore = new TypeMetadataStore();

    private void BuildSymbolTableTypes(SymbolTable? symbolTable)
    {
        // TODO: extract to a symbol table class or another visitor
        if (symbolTable is null)
            throw new ArgumentNullException(nameof(symbolTable));

        BuildTypes(symbolTable.Types);
        BuildFunctionTypes(symbolTable.Functions);
    }

    private void BuildTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
        }
    }

    private void BuildFunctionTypes(IReadOnlyDictionary<string, FunctionSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            var function = symbol.Node;
            if (function.Metadata is not null)
                continue;

            var parameters = new TypeMetadata[function.Parameters.Count];
            for (var i = 0; i < function.Parameters.Count; i++)
            {
                var parameter = function.Parameters[i];
                var type = typeStore.GetType(parameter.Type);

                parameters[i] = type ??
                                throw new TypeCheckerException($"Unknown type '{parameter.Type}'");
            }

            var returnType = typeStore.GetType(function.ReturnType) ??
                             throw new TypeCheckerException($"Unknown type '{function.ReturnType}'");

            // TODO: add function type to types
            function.Metadata = new FunctionMetadata(parameters, returnType);
        }
    }

    private static T? FindInParent<T>(ISyntaxNode node)
        where T : ISyntaxNode
    {
        var function = node.Parent;
        while (function is not null)
        {
            if (function is FunctionDeclarationNode)
                break;

            function = function.Parent;
        }

        return (T?)function;
    }

    public void Visit(BinaryExpressionNode node)
    {
        node.Left.Accept(this);
        node.Right.Accept(this);

        // TODO: more complex logic
        if (node.Left.ReturnTypeMetadata is null || node.Right.ReturnTypeMetadata is null)
            throw new TypeCheckerException();

        if (node.Left.ReturnTypeMetadata != node.Right.ReturnTypeMetadata)
            throw new TypeCheckerException("Binary expressions must have the same return type");

        node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
    }

    public void Visit(BlockStatementNode node)
    {
        BuildSymbolTableTypes(node.SymbolTable);

        foreach (var statement in node.Statements)
            statement.Accept(this);
    }

    public void Visit(CallExpressionNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        // TODO: overload support
        var symbol = node.SymbolTable?.GetFunction(node.FunctionName) ??
                     throw new TypeCheckerException();

        var function = symbol.Node.Metadata ??
                       throw new TypeCheckerException();

        node.Metadata = function;

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var actual = node.Parameters[i].ReturnTypeMetadata;
            var expected = function.ParameterTypes[i];
            if (expected != actual)
                throw new TypeCheckerException($"Expected '{expected}' but got '{actual}'");
        }
    }

    public void Visit(ExpressionStatementNode node)
        => node.Expression.Accept(this);

    public void Visit(FunctionParameterNode node)
    {
        if (node.TypeMetadata is null)
        {
            var type = typeStore.GetType(node.Type);
            if (type is null)
                throw new TypeCheckerException($"Unknown type '{node.Type}'");

            node.TypeMetadata = type;
        }
    }

    public void Visit(FunctionDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.Body?.Accept(this);
    }

    public void Visit(IfStatementNode node)
    {
        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        if (node.Condition.ReturnTypeMetadata != TypeMetadata.Bool)
            throw new TypeCheckerException();
    }

    public void Visit(LiteralExpressionNode node)
    {
        node.ReturnTypeMetadata = node.Kind switch
        {
            // TODO: other types
            LiteralExpressionKind.Number => TypeMetadata.I32,
            LiteralExpressionKind.Boolean => TypeMetadata.Bool,
            LiteralExpressionKind.String => TypeMetadata.String,
            LiteralExpressionKind.Char => TypeMetadata.Char,

            _ => throw new ArgumentOutOfRangeException(),
        };
    }

    public void Visit(ReturnStatementNode node)
    {
        node.Expression.Accept(this);

        var function = FindInParent<FunctionDeclarationNode>(node);
        if (function is null)
            throw new TypeCheckerException();

        if (function.Metadata?.ReturnType != node.Expression.ReturnTypeMetadata)
            throw new TypeCheckerException();
    }

    public void Visit(SyntaxTree node)
    {
        BuildSymbolTableTypes(node.SymbolTable);

        foreach (var statement in node.Functions)
            statement.Accept(this);
    }

    public void Visit(UnaryExpressionNode node)
    {
        node.Operand.Accept(this);

        if (node.Kind == UnaryExpressionKind.UnaryMinus && node.Operand.ReturnTypeMetadata == TypeMetadata.I32)
            node.ReturnTypeMetadata = TypeMetadata.I32;
        else if (node.Kind == UnaryExpressionKind.UnaryPlus && node.Operand.ReturnTypeMetadata == TypeMetadata.I32)
            node.ReturnTypeMetadata = TypeMetadata.I32;
        else if (node.Kind == UnaryExpressionKind.LogicalNot && node.Operand.ReturnTypeMetadata == TypeMetadata.Bool)
            node.ReturnTypeMetadata = TypeMetadata.Bool;
        else
            throw new TypeCheckerException();
    }

    public void Visit(VariableExpressionNode node)
    {
        var symbol = node.SymbolTable?.GetVariable(node.Name);
        if (symbol is null)
            throw new TypeCheckerException();

        node.ReturnTypeMetadata = symbol.Node.TypeMetadata;
    }

    public void Visit(VariableDeclarationStatementNode node)
    {
        node.Expression.Accept(this);

        if (node.TypeMetadata is null)
        {
            var type = typeStore.GetType(node.Type);
            if (type is null)
                throw new TypeCheckerException($"Unknown type '{node.Type}'");

            node.TypeMetadata = type;
        }

        if (node.Expression.ReturnTypeMetadata is null)
            throw new TypeCheckerException();

        if (node.Expression.ReturnTypeMetadata != node.TypeMetadata)
            throw new TypeCheckerException();
    }
}