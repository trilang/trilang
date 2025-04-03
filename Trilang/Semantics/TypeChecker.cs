using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;
using static Trilang.Parsing.Ast.BinaryExpressionKind;
using static Trilang.Parsing.Ast.UnaryExpressionKind;
using static Trilang.Metadata.TypeMetadata;

namespace Trilang.Semantics;

public class TypeChecker : IVisitor
{
    private readonly TypeMetadataStore typeStore;

    public TypeChecker()
        => typeStore = new TypeMetadataStore();

    private void BuildSymbolTableTypes(ISymbolTable? symbolTable)
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
            if (symbol.IsArray)
            {
                var type = typeStore.GetType(symbol.Name[..^2]) ??
                           throw new TypeCheckerException($"Unknown type '{symbol.Name}'");

                typeStore.DefineType(symbol.Name, type);
            }
        }
    }

    private void BuildFunctionTypes(IReadOnlyDictionary<string, FunctionSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            var function = symbol.Node;
            if (function.Metadata is not null)
                continue;

            var parameters = new IMetadata[function.Parameters.Count];
            for (var i = 0; i < function.Parameters.Count; i++)
            {
                var parameter = function.Parameters[i];
                var type = typeStore.GetType(parameter.Type.Name);

                parameters[i] = type ??
                                throw new TypeCheckerException($"Unknown type '{parameter.Type}'");
            }

            var returnType = typeStore.GetType(function.ReturnType.Name) ??
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

    public void Visit(ArrayAccessExpressionNode node)
    {
        node.Member.Accept(this);
        node.Index.Accept(this);

        if (!Equals(node.Index.ReturnTypeMetadata, I32))
            throw new TypeCheckerException();
    }

    public void Visit(BinaryExpressionNode node)
    {
        node.Left.Accept(this);
        node.Right.Accept(this);

        // TODO: more complex logic
        if (node.Kind == BinaryExpressionKind.Unknown ||
            node.Left.ReturnTypeMetadata is null ||
            node.Right.ReturnTypeMetadata is null)
            throw new TypeCheckerException();

        if (node.Kind is Addition or Subtraction or Multiplication or Division or Modulus &&
            ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
             (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
             (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
             (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
             (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
             (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
             (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
             (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64)) ||
             (Equals(node.Left.ReturnTypeMetadata, F32) && Equals(node.Right.ReturnTypeMetadata, F32)) ||
             (Equals(node.Left.ReturnTypeMetadata, F64) && Equals(node.Right.ReturnTypeMetadata, F64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAnd or BitwiseOr or BitwiseXor &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is ConditionalAnd or ConditionalOr &&
                 Equals(node.Left.ReturnTypeMetadata, Bool) && Equals(node.Right.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind is Equality or Inequality or
                     LessThan or LessThanOrEqual or
                     GreaterThan or GreaterThanOrEqual &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind is Assignment or AdditionAssignment or
                     SubtractionAssignment or MultiplicationAssignment or
                     DivisionAssignment or ModulusAssignment &&
                 node.Left is MemberAccessExpressionNode &&
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64) ||
                  Equals(node.Right.ReturnTypeMetadata, F32) ||
                  Equals(node.Right.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAndAssignment or BitwiseOrAssignment or BitwiseXorAssignment &&
                 node.Left is MemberAccessExpressionNode &&
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else
        {
            throw new TypeCheckerException();
        }
    }

    public void Visit(BlockStatementNode node)
    {
        foreach (var statement in node.Statements)
            statement.Accept(this);
    }

    public void Visit(BreakNode node)
    {
    }

    public void Visit(CallExpressionNode node)
    {
        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        // TODO: overload support
        var symbol = node.SymbolTable?.GetFunction(node.Member.Name) ??
                     throw new TypeCheckerException();

        var function = symbol.Node.Metadata ??
                       throw new TypeCheckerException();

        node.Metadata = function;

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var actual = node.Parameters[i].ReturnTypeMetadata;
            var expected = function.ParameterTypes[i];
            if (!expected.Equals(actual))
                throw new TypeCheckerException($"Expected '{expected}' but got '{actual}'");
        }
    }

    public void Visit(ContinueNode node)
    {
    }

    public void Visit(ExpressionStatementNode node)
        => node.Expression.Accept(this);

    public void Visit(FunctionParameterNode node)
    {
        if (node.Type.Metadata is null)
        {
            var type = typeStore.GetType(node.Type.Name);
            if (type is null)
                throw new TypeCheckerException($"Unknown type '{node.Type}'");

            node.Type.Metadata = type;
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

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new TypeCheckerException();
    }

    public void Visit(LiteralExpressionNode node)
    {
        node.ReturnTypeMetadata = node.Kind switch
        {
            // TODO: other types
            LiteralExpressionKind.Number => I32,
            LiteralExpressionKind.Boolean => Bool,
            LiteralExpressionKind.String => TypeMetadata.String,
            LiteralExpressionKind.Char => TypeMetadata.Char,

            _ => throw new ArgumentOutOfRangeException(),
        };
    }

    public void Visit(MemberAccessExpressionNode node)
    {
        var symbol = node.SymbolTable?.GetVariable(node.Name) ??
                     throw new TypeCheckerException();

        node.ReturnTypeMetadata = symbol.Node.Type.Metadata;
    }

    public void Visit(ReturnStatementNode node)
    {
        node.Expression.Accept(this);

        var function = FindInParent<FunctionDeclarationNode>(node);
        if (function is null)
            throw new TypeCheckerException();

        if (!Equals(function.Metadata?.ReturnType, node.Expression.ReturnTypeMetadata))
            throw new TypeCheckerException();
    }

    public void Visit(SyntaxTree node)
    {
        BuildSymbolTableTypes(node.SymbolTable);

        foreach (var statement in node.Functions)
            statement.Accept(this);
    }

    public void Visit(TypeNode node)
    {
        var type = typeStore.GetType(node.Name);
        if (type is null)
            throw new TypeCheckerException($"Unknown type '{node.Name}'");

        node.Metadata = type;
    }

    public void Visit(UnaryExpressionNode node)
    {
        node.Operand.Accept(this);

        // TODO: more complex logic
        if (node.Kind == UnaryExpressionKind.Unknown || node.Operand.ReturnTypeMetadata is null)
            throw new TypeCheckerException();

        if (node.Kind == UnaryMinus &&
            (Equals(node.Operand.ReturnTypeMetadata, I8) ||
             Equals(node.Operand.ReturnTypeMetadata, I16) ||
             Equals(node.Operand.ReturnTypeMetadata, I32) ||
             Equals(node.Operand.ReturnTypeMetadata, I64) ||
             Equals(node.Operand.ReturnTypeMetadata, U8) ||
             Equals(node.Operand.ReturnTypeMetadata, U16) ||
             Equals(node.Operand.ReturnTypeMetadata, U32) ||
             Equals(node.Operand.ReturnTypeMetadata, U64) ||
             Equals(node.Operand.ReturnTypeMetadata, F32) ||
             Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == UnaryPlus &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64) ||
                  Equals(node.Operand.ReturnTypeMetadata, F32) ||
                  Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == LogicalNot && Equals(node.Operand.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind == BitwiseNot &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else
        {
            throw new TypeCheckerException();
        }
    }

    public void Visit(VariableDeclarationStatementNode node)
    {
        node.Expression.Accept(this);

        if (node.Type.Metadata is null)
        {
            var type = typeStore.GetType(node.Type.Name);
            if (type is null)
                throw new TypeCheckerException($"Unknown type '{node.Type}'");

            node.Type.Metadata = type;
        }

        if (node.Expression.ReturnTypeMetadata is null)
            throw new TypeCheckerException();

        if (!node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
            throw new TypeCheckerException();
    }

    public void Visit(WhileNode node)
    {
        node.Condition.Accept(this);
        node.Body.Accept(this);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new TypeCheckerException("Condition must be a boolean");
    }
}