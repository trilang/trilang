using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;
using static Trilang.Parsing.Ast.UnaryExpressionKind;
using static Trilang.Metadata.TypeMetadata;

namespace Trilang.Semantics;

internal class TypeChecker : IVisitor
{
    private readonly TypeMetadataProvider typeProvider;

    public TypeChecker() : this(new TypeMetadataProvider())
    {
    }

    public TypeChecker(TypeMetadataProvider typeProvider)
        => this.typeProvider = typeProvider;

    public void Visit(ArrayAccessExpressionNode node)
    {
        node.Member.Accept(this);
        node.Index.Accept(this);

        if (!Equals(node.Index.ReturnTypeMetadata, I32))
            throw new SemanticAnalysisException("Array index must be of type i32");
    }

    public void Visit(BinaryExpressionNode node)
    {
        node.Left.Accept(this);
        node.Right.Accept(this);

        // TODO: more complex logic
        if (node.Kind == BinaryExpressionKind.Unknown ||
            node.Left.ReturnTypeMetadata is null ||
            node.Right.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException("Invalid binary expression: unknown kind or operand types");

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
            throw new SemanticAnalysisException($"Invalid binary expression: incompatible operand types '{node.Left.ReturnTypeMetadata}' and '{node.Right.ReturnTypeMetadata}' for operator '{node.Kind}'");
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

        if (node.Member.ReturnTypeMetadata is not FunctionTypeMetadata function)
            throw new SemanticAnalysisException("Cannot call a non-function member");

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var actual = node.Parameters[i].ReturnTypeMetadata;
            var expected = function.ParameterTypes[i];
            if (!expected.Equals(actual))
                throw new SemanticAnalysisException($"Expected '{expected}' but got '{actual}'");
        }
    }

    public void Visit(ConstructorDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetConstructor(node.Parameters.Select(x => x.Type.Metadata!));

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

        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetField(node.Name);
    }

    public void Visit(FunctionDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        // we can be sure that node.Metadata is not null here
        // because it is set as a part of TypeNode
        var functionType = new FunctionTypeMetadata(
            node.Parameters.Select(parameter => parameter.Type.Metadata!).ToList(),
            node.ReturnType.Metadata!);
        node.Metadata = new FunctionMetadata(node.Name, functionType);

        node.Body?.Accept(this);
    }

    public void Visit(FunctionTypeNode node)
    {
        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this);

        node.ReturnType.Accept(this);

        node.Metadata = new FunctionTypeMetadata(
            node.ParameterTypes.Select(parameter => parameter.Metadata!).ToList(),
            node.ReturnType.Metadata!);
    }

    public void Visit(IfStatementNode node)
    {
        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("The condition returns non-boolean type.");
    }

    public void Visit(InterfaceNode node)
    {
        var metadata = typeProvider.GetType(node.Name) as InterfaceMetadata ??
                       throw new SemanticAnalysisException($"Unknown interface type '{node.Name}'");

        node.Metadata = metadata;

        foreach (var field in node.Fields)
            field.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);
    }

    public void Visit(InterfaceFieldNode node)
    {
        node.Type.Accept(this);

        var type = (InterfaceMetadata)((InterfaceNode)node.Parent!).Metadata!;
        node.Metadata = type.GetField(node.Name);
    }

    public void Visit(InterfaceMethodNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        var type = (InterfaceMetadata)((InterfaceNode)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);
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

            _ => throw new ArgumentOutOfRangeException(nameof(node.Kind), $"Unsupported literal expression kind: {node.Kind}"),
        };
    }

    public void Visit(MemberAccessExpressionNode node)
    {
        if (node.Member is null)
        {
            var symbol = node.SymbolTable?.GetId(node.Name) ??
                         throw new SemanticAnalysisException($"Symbol '{node.Name}' not found in current scope");

            node.ReturnTypeMetadata = symbol.Node switch
            {
                FieldDeclarationNode fieldNode
                    => fieldNode.Type.Metadata,

                VariableDeclarationStatementNode variableStatementNode
                    => variableStatementNode.Type.Metadata,

                ParameterNode parameterNode
                    => parameterNode.Type.Metadata,

                FunctionDeclarationNode functionNode
                    => functionNode.Metadata?.TypeMetadata,

                MethodDeclarationNode methodNode
                    => methodNode.Metadata?.TypeMetadata,

                InterfaceFieldNode interfaceFieldNode
                    => interfaceFieldNode.Type.Metadata,

                InterfaceMethodNode interfaceMethodNode
                    => interfaceMethodNode.Metadata?.TypeMetadata,

                TypeDeclarationNode typeDeclarationNode
                    => typeDeclarationNode.Metadata,

                _ => throw new SemanticAnalysisException(),
            };
        }
        else
        {
            node.Member.Accept(this);

            var returnTypeMetadata = node.Member.ReturnTypeMetadata;
            if (returnTypeMetadata is TypeAliasMetadata alias)
                returnTypeMetadata = alias.Type;

            node.ReturnTypeMetadata = returnTypeMetadata switch
            {
                TypeMetadata type
                    => type.GetField(node.Name)?.Type ??
                       type.GetMethod(node.Name)?.TypeMetadata ??
                       throw new SemanticAnalysisException($"Cannot find member '{node.Name}' in type '{node.Member.ReturnTypeMetadata!.Name}'"),

                InterfaceMetadata @interface
                    => @interface.GetField(node.Name)?.Type ??
                       @interface.GetMethod(node.Name)?.TypeMetadata ??
                       throw new SemanticAnalysisException($"Cannot find member '{node.Name}' in interface '{node.Member.ReturnTypeMetadata!.Name}'"),

                _ => node.ReturnTypeMetadata
            };
        }

        if (node.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine return type for member '{node.Name}'");
    }

    public void Visit(MethodDeclarationNode node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);

        node.Body.Accept(this);
    }

    public void Visit(ReturnStatementNode node)
    {
        node.Expression?.Accept(this);

        var method = node.FindInParent<MethodDeclarationNode>();
        if (method is not null)
        {
            if (!Equals(method.Metadata?.TypeMetadata.ReturnType, node.Expression?.ReturnTypeMetadata))
                throw new SemanticAnalysisException($"Method return type mismatch: expected '{method.Metadata?.TypeMetadata.ReturnType}', got '{node.Expression?.ReturnTypeMetadata}'");
        }
        else
        {
            var constructor = node.FindInParent<ConstructorDeclarationNode>();
            if (constructor is not null)
            {
                if (!Equals(TypeMetadata.Void, node.Expression?.ReturnTypeMetadata ?? TypeMetadata.Void))
                    throw new SemanticAnalysisException($"Constructor return type mismatch: expected '{TypeMetadata.Void.Name}', got '{node.Expression?.ReturnTypeMetadata}'");
            }
            else
            {
                var function = node.FindInParent<FunctionDeclarationNode>();
                if (function is null)
                    throw new SemanticAnalysisException("Return statement must be inside a function, method, or constructor");

                if (!Equals(function.Metadata?.TypeMetadata.ReturnType, node.Expression?.ReturnTypeMetadata ?? TypeMetadata.Void))
                    throw new SemanticAnalysisException($"Function return type mismatch: expected '{function.Metadata?.TypeMetadata.ReturnType}', got '{node.Expression?.ReturnTypeMetadata}'");
            }
        }
    }

    public void Visit(ParameterNode node)
        => node.Type.Accept(this);

    public void Visit(SyntaxTree node)
    {
        foreach (var statement in node.Declarations)
            statement.Accept(this);
    }

    public void Visit(TypeAliasDeclarationNode node)
    {
        node.Type.Accept(this);

        node.Metadata = typeProvider.GetType(node.Name) ??
                        throw new SemanticAnalysisException($"Unknown type '{node.Name}'");
    }

    public void Visit(TypeDeclarationNode node)
    {
        var metadata = typeProvider.GetType(node.Name);
        if (metadata is not TypeMetadata type)
            throw new SemanticAnalysisException($"The '{node.Name}' type is not a type.");

        node.Metadata = type;

        foreach (var field in node.Fields)
            field.Accept(this);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);
    }

    public void Visit(TypeNode node)
    {
        var type = typeProvider.GetType(node.Name) ??
                   throw new SemanticAnalysisException($"Referenced unknown type '{node.Name}'");

        node.Metadata = type;
    }

    public void Visit(UnaryExpressionNode node)
    {
        node.Operand.Accept(this);

        // TODO: more complex logic
        if (node.Kind == UnaryExpressionKind.Unknown || node.Operand.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException("Invalid unary expression: unknown kind or operand type");

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
            throw new SemanticAnalysisException($"Invalid unary expression: incompatible operand type '{node.Operand.ReturnTypeMetadata}' for operator '{node.Kind}'");
        }
    }

    public void Visit(VariableDeclarationStatementNode node)
    {
        node.Type.Accept(this);
        node.Expression.Accept(this);

        if (node.Expression.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine type of expression in variable declaration '{node.Name}'");

        if (!node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
            throw new SemanticAnalysisException($"Type mismatch in variable declaration '{node.Name}': expected '{node.Type.Metadata}', got '{node.Expression.ReturnTypeMetadata}'");
    }

    public void Visit(WhileNode node)
    {
        node.Condition.Accept(this);
        node.Body.Accept(this);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("Condition must be a boolean");
    }
}