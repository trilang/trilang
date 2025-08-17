using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class ReplacePropertyFieldAndValueWithGeneratedField : ITransformer
{
    private FieldMetadata? currentField;
    private MethodMetadata? currentSetter;

    public ISyntaxNode TransformArrayAccess(ArrayAccessExpressionNode node)
    {
        var member = (IExpressionNode)node.Member.Transform(this);
        var index = (IExpressionNode)node.Index.Transform(this);
        if (ReferenceEquals(member, node.Member) && ReferenceEquals(index, node.Index))
            return node;

        return new ArrayAccessExpressionNode(member, index)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformArrayType(ArrayTypeNode node)
        => node;

    public ISyntaxNode TransformAsExpression(AsExpressionNode node)
    {
        var expression = (IExpressionNode)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new AsExpressionNode(expression, node.Type)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformBinaryExpression(BinaryExpressionNode node)
    {
        var left = (IExpressionNode)node.Left.Transform(this);
        var right = (IExpressionNode)node.Right.Transform(this);
        if (ReferenceEquals(left, node.Left) && ReferenceEquals(right, node.Right))
            return node;

        return new BinaryExpressionNode(node.Kind, left, right)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformBlock(BlockStatementNode node)
    {
        for (var i = 0; i < node.Statements.Count; i++)
        {
            var odlStatement = node.Statements[i];
            var newStatement = (IStatementNode)odlStatement.Transform(this);
            if (ReferenceEquals(newStatement, odlStatement))
                continue;

            node.Replace(odlStatement, newStatement);
        }

        return node;
    }

    public ISyntaxNode TransformBreak(BreakNode node)
        => node;

    public ISyntaxNode TransformCall(CallExpressionNode node)
    {
        var member = (IExpressionNode)node.Member.Transform(this);

        var changed = false;
        var parameters = new IExpressionNode[node.Parameters.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            if (ReferenceEquals(node.Parameters[i], parameters[i]))
                changed = true;

            parameters[i] = (IExpressionNode)node.Parameters[i].Transform(this);
        }

        if (ReferenceEquals(member, node.Member) && !changed)
            return node;

        return new CallExpressionNode(member, parameters);
    }

    public ISyntaxNode TransformConstructor(ConstructorDeclarationNode node)
        => node;

    public ISyntaxNode TransformContinue(ContinueNode node)
        => node;

    public ISyntaxNode TransformDiscriminatedUnion(DiscriminatedUnionNode node)
        => node;

    public ISyntaxNode TransformExpressionBlock(ExpressionBlockNode node)
        => node;

    public ISyntaxNode TransformExpressionStatement(ExpressionStatementNode node)
    {
        var expression = (IExpressionNode)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new ExpressionStatementNode(expression)
        {
            SymbolTable = node.SymbolTable,
        };
    }

    public ISyntaxNode TransformFunction(FunctionDeclarationNode node)
        => node;

    public ISyntaxNode TransformFunctionType(FunctionTypeNode node)
        => node;

    public ISyntaxNode TransformGenericType(GenericTypeNode node)
        => node;

    public ISyntaxNode TransformGoTo(GoToNode node)
        => node;

    public ISyntaxNode TransformIfDirective(IfDirectiveNode node)
        => node;

    public ISyntaxNode TransformIf(IfStatementNode node)
    {
        var condition = (IExpressionNode)node.Condition.Transform(this);
        var then = (BlockStatementNode)node.Then.Transform(this);
        var @else = (BlockStatementNode?)node.Else?.Transform(this);

        if (ReferenceEquals(condition, node.Condition))
            return node;

        return new IfStatementNode(condition, then, @else)
        {
            SymbolTable = node.SymbolTable,
        };
    }

    public ISyntaxNode TransformInterface(InterfaceNode node)
        => node;

    public ISyntaxNode TransformInterfaceProperty(InterfacePropertyNode node)
        => node;

    public ISyntaxNode TransformInterfaceMethod(InterfaceMethodNode node)
        => node;

    public ISyntaxNode TransformLabel(LabelNode node)
        => node;

    public ISyntaxNode TransformLiteral(LiteralExpressionNode node)
        => node;

    public ISyntaxNode TransformMemberAccess(MemberAccessExpressionNode node)
    {
        if (node.Member is not null)
            return node;

        if (node.IsField)
        {
            Debug.Assert(currentField is not null);

            var thisMember = new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
            {
                Reference = new ParameterMetadata(MemberAccessExpressionNode.This, currentField.DeclaringType),
                AccessKind = PropertyAccessKind.Read,
            };

            return new MemberAccessExpressionNode(thisMember, currentField.Name)
            {
                Reference = currentField,
                AccessKind = node.AccessKind,
            };
        }

        if (node.IsValue)
        {
            Debug.Assert(currentSetter is not null);

            var parameter = currentSetter.Parameters.First(x => x.Name == MemberAccessExpressionNode.Value);

            return new MemberAccessExpressionNode(parameter.Name)
            {
                Reference = parameter,
                AccessKind = node.AccessKind,
            };
        }

        return node;
    }

    public ISyntaxNode TransformMethod(MethodDeclarationNode node)
        => node;

    public ISyntaxNode TransformNewArray(NewArrayExpressionNode node)
    {
        var size = (IExpressionNode)node.Size.Transform(this);
        if (ReferenceEquals(size, node.Size))
            return node;

        return new NewArrayExpressionNode(node.Type, size)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformNewObject(NewObjectExpressionNode node)
    {
        var changed = false;
        var parameters = new IExpressionNode[node.Parameters.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            if (ReferenceEquals(node.Parameters[i], parameters[i]))
                changed = true;

            parameters[i] = (IExpressionNode)node.Parameters[i].Transform(this);
        }

        if (!changed)
            return node;

        return new NewObjectExpressionNode(node.Type, parameters)
        {
            SymbolTable = node.SymbolTable,
            Metadata = node.Metadata,
        };
    }

    public ISyntaxNode TransformNull(NullExpressionNode node)
        => node;

    public ISyntaxNode TransformParameter(ParameterNode node)
        => node;

    public ISyntaxNode TransformProperty(PropertyDeclarationNode node)
    {
        var propertyMetadata = node.Metadata!;
        var returnTypeMetadata = propertyMetadata.Type;
        var typeMetadata = (TypeMetadata)propertyMetadata.DeclaringType;

        currentField = new FieldMetadata(typeMetadata, $"<>_{propertyMetadata.Name}", returnTypeMetadata);
        typeMetadata.AddField(currentField);

        node.Getter?.Transform(this);
        node.Setter?.Transform(this);

        currentField = null;

        return node;
    }

    public ISyntaxNode TransformGetter(PropertyGetterNode node)
    {
        node.Body?.Transform(this);

        return node;
    }

    public ISyntaxNode TransformSetter(PropertySetterNode node)
    {
        currentSetter = node.Metadata!;

        node.Body?.Transform(this);

        currentSetter = null;

        return node;
    }

    public ISyntaxNode TransformReturn(ReturnStatementNode node)
    {
        if (node.Expression is null)
            return node;

        var expression = (IExpressionNode)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new ReturnStatementNode(expression)
        {
            SymbolTable = node.SymbolTable,
        };
    }

    public ISyntaxNode TransformTree(SyntaxTree node)
    {
        foreach (var declaration in node.Declarations)
            declaration.Transform(this);

        return node;
    }

    public ISyntaxNode TransformTuple(TupleExpressionNode node)
    {
        var changed = false;
        var expressions = new IExpressionNode[node.Expressions.Count];
        for (var i = 0; i < expressions.Length; i++)
        {
            if (ReferenceEquals(node.Expressions[i], expressions[i]))
                changed = true;

            expressions[i] = (IExpressionNode)node.Expressions[i].Transform(this);
        }

        if (!changed)
            return node;

        return new TupleExpressionNode(expressions)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformTupleType(TupleTypeNode node)
        => node;

    public ISyntaxNode TransformTypeAlias(TypeAliasDeclarationNode node)
        => node;

    public ISyntaxNode TransformType(TypeDeclarationNode node)
    {
        foreach (var constructor in node.Constructors)
            constructor.Transform(this);

        foreach (var method in node.Methods)
            method.Transform(this);

        foreach (var property in node.Properties)
            property.Transform(this);

        return node;
    }

    public ISyntaxNode TransformTypeNode(TypeNode node)
        => node;

    public ISyntaxNode TransformUnaryExpression(UnaryExpressionNode node)
    {
        var operand = (IExpressionNode)node.Operand.Transform(this);
        if (ReferenceEquals(operand, node.Operand))
            return node;

        return new UnaryExpressionNode(node.Kind, operand)
        {
            SymbolTable = node.SymbolTable,
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISyntaxNode TransformVariable(VariableDeclarationStatementNode node)
    {
        var expression = (IExpressionNode)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new VariableDeclarationStatementNode(node.Name, node.Type, expression)
        {
            SymbolTable = node.SymbolTable,
        };
    }

    public ISyntaxNode TransformWhile(WhileNode node)
    {
        var condition = (IExpressionNode)node.Condition.Transform(this);
        var body = (BlockStatementNode)node.Body.Transform(this);
        if (ReferenceEquals(condition, node.Condition))
            return node;

        return new WhileNode(condition, body)
        {
            SymbolTable = node.SymbolTable,
        };
    }
}