using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;
using static Trilang.Parsing.Ast.UnaryExpressionKind;
using static Trilang.Metadata.TypeMetadata;

namespace Trilang.Semantics;

public class TypeChecker : IVisitor
{
    private readonly TypeMetadataProvider typeProvider;

    public TypeChecker() : this(new TypeMetadataProvider())
    {
    }

    public TypeChecker(TypeMetadataProvider typeProvider)
        => this.typeProvider = typeProvider;

    private static T? FindInParent<T>(ISyntaxNode node)
        where T : ISyntaxNode
    {
        var parent = node.Parent;
        while (parent is not null)
        {
            if (parent is T result)
                return result;

            parent = parent.Parent;
        }

        return default;
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
        // TODO: overload support
        var symbol = node.SymbolTable?.GetFunction(node.Member.Name) ??
                     throw new TypeCheckerException();

        var function = symbol.Node.Metadata ??
                       throw new TypeCheckerException();

        node.Metadata = function;

        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var actual = node.Parameters[i].ReturnTypeMetadata;
            var expected = function.TypeMetadata.ParameterTypes[i];
            if (!expected.Equals(actual))
                throw new TypeCheckerException($"Expected '{expected}' but got '{actual}'");
        }
    }

    public void Visit(ConstructorDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.Body.Accept(this);
    }

    public void Visit(ContinueNode node)
    {
    }

    public void Visit(ExpressionStatementNode node)
        => node.Expression.Accept(this);

    public void Visit(FieldDeclarationNode node)
    {
        node.Type.Accept(this);
    }

    public void Visit(FunctionDeclarationNode node)
    {
        var parameters = new ITypeMetadata[node.Parameters.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            var parameter = node.Parameters[i];
            var type = typeProvider.GetType(parameter.Type.Name) ??
                       throw new TypeCheckerException($"Unknown type '{parameter.Type.Name}'");

            parameters[i] = type;
        }

        var returnType = typeProvider.GetType(node.ReturnType.Name) ??
                         throw new TypeCheckerException($"Unknown type '{node.ReturnType.Name}'");

        var functionTypeMetadata = new FunctionTypeMetadata(parameters, returnType);
        functionTypeMetadata = typeProvider.GetType(functionTypeMetadata.Name) as FunctionTypeMetadata ??
                               throw new TypeCheckerException($"Unknown type '{functionTypeMetadata.Name}'");

        node.Metadata = new FunctionMetadata(node.Name, functionTypeMetadata);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);
        node.Body?.Accept(this);
    }

    public void Visit(FunctionTypeNode node)
    {
        var parameters = new ITypeMetadata[node.ParameterTypes.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            var parameter = node.ParameterTypes[i];
            var type = typeProvider.GetType(parameter.Name) ??
                       throw new TypeCheckerException($"Unknown type '{parameter.Name}'");

            parameters[i] = type;
        }

        var returnType = typeProvider.GetType(node.ReturnType.Name) ??
                         throw new TypeCheckerException($"Unknown type '{node.ReturnType.Name}'");

        node.Metadata = new FunctionTypeMetadata(parameters, returnType);

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this);

        node.ReturnType.Accept(this);
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

    public void Visit(MethodDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);
    }

    public void Visit(ReturnStatementNode node)
    {
        node.Expression?.Accept(this);

        if (node.Expression is not null && FindInParent<ConstructorDeclarationNode>(node) is not null)
            throw new TypeCheckerException();

        var method = FindInParent<MethodDeclarationNode>(node);
        if (method is not null)
        {
            if (!Equals(method.Metadata?.TypeMetadata.ReturnType, node.Expression?.ReturnTypeMetadata))
                throw new TypeCheckerException();
        }
        else
        {
            var constructor = FindInParent<ConstructorDeclarationNode>(node);
            if (constructor is not null)
            {
                if (!Equals(constructor.Metadata?.DeclaringType, node.Expression?.ReturnTypeMetadata))
                    throw new TypeCheckerException();
            }
            else
            {
                var function = FindInParent<FunctionDeclarationNode>(node);
                if (function is null)
                    throw new TypeCheckerException();

                if (!Equals(function.Metadata?.TypeMetadata.ReturnType, node.Expression?.ReturnTypeMetadata))
                    throw new TypeCheckerException();
            }
        }
    }

    public void Visit(ParameterNode node)
    {
        if (node.Type.Metadata is not null)
            return;

        var type = typeProvider.GetType(node.Type.Name) ??
                   throw new TypeCheckerException($"Unknown type '{node.Type}'");

        node.Type.Metadata = type;
    }

    public void Visit(SyntaxTree node)
    {
        foreach (var statement in node.Declarations)
            statement.Accept(this);
    }

    public void Visit(TypeAliasDeclarationNode node)
    {
        node.Type.Accept(this);

        node.Metadata = typeProvider.GetType(node.Name) ??
                        throw new TypeCheckerException($"Unknown type '{node.Name}'");
    }

    public void Visit(TypeDeclarationNode node)
    {
        var metadata = typeProvider.GetType(node.Name);
        if (metadata is not TypeMetadata type)
            throw new TypeCheckerException($"Unknown type '{node.Name}'");

        foreach (var field in node.Fields)
        {
            field.Metadata = type.GetField(field.Name);
            field.Accept(this);
        }

        foreach (var constructor in node.Constructors)
        {
            var parameters = new ITypeMetadata[constructor.Parameters.Count];
            for (var i = 0; i < constructor.Parameters.Count; i++)
            {
                var parameter = constructor.Parameters[i];
                var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                    throw new TypeCheckerException($"Unknown type '{parameter.Type.Name}'");

                parameters[i] = parameterType;
            }

            constructor.Metadata = type.GetConstructor(parameters);
            constructor.Accept(this);
        }

        foreach (var method in node.Methods)
        {
            method.Metadata = type.GetMethod(method.Name);
            method.Accept(this);
        }

        node.Metadata = type;
    }

    public void Visit(TypeNode node)
    {
        var type = typeProvider.GetType(node.Name) ??
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
        if (node.Type.Metadata is null)
        {
            var type = typeProvider.GetType(node.Type.Name) ??
                       throw new TypeCheckerException($"Unknown type '{node.Type}'");

            node.Type.Metadata = type;
        }

        node.Expression.Accept(this);

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