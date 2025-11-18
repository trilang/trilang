using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Lower;

internal class ReplaceCompoundAssignments : ITransformer<ISemanticNode>
{
    public ISemanticNode TransformArrayAccess(ArrayAccessExpression node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var index = (IExpression)node.Index.Transform(this);
        if (ReferenceEquals(member, node.Member) && ReferenceEquals(index, node.Index))
            return node;

        return new ArrayAccessExpression(null, member, index)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISemanticNode TransformArrayType(ArrayType node)
        => node;

    public ISemanticNode TransformBinaryExpression(BinaryExpression node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);

        if (node.IsCompoundAssignment)
        {
            var kind = node.Kind switch
            {
                AdditionAssignment => Addition,
                SubtractionAssignment => Subtraction,
                MultiplicationAssignment => Multiplication,
                DivisionAssignment => Division,
                ModulusAssignment => Modulus,
                BitwiseAndAssignment => BitwiseAnd,
                BitwiseOrAssignment => BitwiseOr,
                BitwiseXorAssignment => BitwiseXor,
                _ => throw new ArgumentOutOfRangeException(),
            };

            var read = (MemberAccessExpression)left.Clone();
            read.AccessKind = MemberAccessKind.Read;

            var write = (MemberAccessExpression)left.Clone();
            write.AccessKind = MemberAccessKind.Write;

            right = new BinaryExpression(null, kind, read, right)
            {
                ReturnTypeMetadata = node.ReturnTypeMetadata,
            };

            return new BinaryExpression(null, Assignment, write, right)
            {
                ReturnTypeMetadata = node.ReturnTypeMetadata,
            };
        }

        if (ReferenceEquals(left, node.Left) && ReferenceEquals(right, node.Right))
            return node;

        return new BinaryExpression(null, node.Kind, left, right)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISemanticNode TransformBlock(BlockStatement node)
    {
        for (var i = 0; i < node.Statements.Count; i++)
        {
            var odlStatement = node.Statements[i];
            var newStatement = (IStatement)odlStatement.Transform(this);
            if (ReferenceEquals(newStatement, odlStatement))
                continue;

            node.Replace(odlStatement, newStatement);
        }

        return node;
    }

    public ISemanticNode TransformBreak(Break node)
        => node;

    public ISemanticNode TransformCall(CallExpression node)
    {
        var member = (IExpression)node.Member.Transform(this);

        var changed = false;
        var parameters = new IExpression[node.Parameters.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            if (ReferenceEquals(node.Parameters[i], parameters[i]))
                changed = true;

            parameters[i] = (IExpression)node.Parameters[i].Transform(this);
        }

        if (ReferenceEquals(member, node.Member) && !changed)
            return node;

        return new CallExpression(null, member, parameters);
    }

    public ISemanticNode TransformCast(CastExpression node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new CastExpression(null, node.Type, expression);
    }

    public ISemanticNode TransformConstructor(ConstructorDeclaration node)
    {
        node.Body.Transform(this);

        return node;
    }

    public ISemanticNode TransformContinue(Continue node)
        => node;

    public ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnion node)
        => node;

    public ISemanticNode TransformExpressionBlock(ExpressionBlock node)
        => node;

    public ISemanticNode TransformExpressionStatement(ExpressionStatement node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new ExpressionStatement(null, expression);
    }

    public ISemanticNode TransformFakeDeclaration(FakeDeclaration node)
        => node;

    public ISemanticNode TransformFakeExpression(FakeExpression node)
        => node;

    public ISemanticNode TransformFakeStatement(FakeStatement node)
        => node;

    public ISemanticNode TransformFakeType(FakeType node)
        => node;

    public ISemanticNode TransformFunction(FunctionDeclaration node)
    {
        node.Body.Transform(this);

        return node;
    }

    public ISemanticNode TransformFunctionType(FunctionType node)
        => node;

    public ISemanticNode TransformGenericType(GenericType node)
        => node;

    public ISemanticNode TransformGoTo(GoTo node)
        => node;

    public ISemanticNode TransformIfDirective(IfDirective node)
        => node;

    public ISemanticNode TransformIf(IfStatement node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var then = (BlockStatement)node.Then.Transform(this);
        var @else = (BlockStatement?)node.Else?.Transform(this);

        if (ReferenceEquals(condition, node.Condition))
            return node;

        return new IfStatement(null, condition, then, @else);
    }

    public ISemanticNode TransformInterface(Interface node)
        => node;

    public ISemanticNode TransformInterfaceProperty(InterfaceProperty node)
        => node;

    public ISemanticNode TransformInterfaceMethod(InterfaceMethod node)
        => node;

    public ISemanticNode TransformAsExpression(IsExpression node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new IsExpression(null, expression, node.Type);
    }

    public ISemanticNode TransformLabel(Label node)
        => node;

    public ISemanticNode TransformLiteral(LiteralExpression node)
        => node;

    public ISemanticNode TransformMemberAccess(MemberAccessExpression node)
    {
        if (node.IsFirstMember)
            return node;

        var member = (IExpression)node.Member.Transform(this);
        if (ReferenceEquals(member, node.Member))
            return node;

        return new MemberAccessExpression(null, member, node.Name)
        {
            Reference = node.Reference,
        };
    }

    public ISemanticNode TransformMethod(MethodDeclaration node)
    {
        node.Body.Transform(this);

        return node;
    }

    public ISemanticNode TransformNamespace(Namespace node)
        => node;

    public ISemanticNode TransformNewArray(NewArrayExpression node)
    {
        var size = (IExpression)node.Size.Transform(this);
        if (ReferenceEquals(size, node.Size))
            return node;

        return new NewArrayExpression(null, node.Type, size)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISemanticNode TransformNewObject(NewObjectExpression node)
    {
        var changed = false;
        var parameters = new IExpression[node.Parameters.Count];
        for (var i = 0; i < parameters.Length; i++)
        {
            if (ReferenceEquals(node.Parameters[i], parameters[i]))
                changed = true;

            parameters[i] = (IExpression)node.Parameters[i].Transform(this);
        }

        if (!changed)
            return node;

        return new NewObjectExpression(null, node.Type, parameters)
        {
            Metadata = node.Metadata,
        };
    }

    public ISemanticNode TransformNull(NullExpression node)
        => node;

    public ISemanticNode TransformParameter(Parameter node)
        => node;

    public ISemanticNode TransformProperty(PropertyDeclaration node)
    {
        node.Getter?.Transform(this);
        node.Setter?.Transform(this);

        return node;
    }

    public ISemanticNode TransformGetter(PropertyGetter node)
    {
        node.Body?.Transform(this);

        return node;
    }

    public ISemanticNode TransformSetter(PropertySetter node)
    {
        node.Body?.Transform(this);

        return node;
    }

    public ISemanticNode TransformReturn(ReturnStatement node)
    {
        if (node.Expression is null)
            return node;

        var expression = (IExpression)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new ReturnStatement(null, expression);
    }

    public ISemanticNode TransformTree(SemanticTree node)
    {
        foreach (var declaration in node.Declarations)
            declaration.Transform(this);

        return node;
    }

    public ISemanticNode TransformTuple(TupleExpression node)
    {
        var changed = false;
        var expressions = new IExpression[node.Expressions.Count];
        for (var i = 0; i < expressions.Length; i++)
        {
            if (ReferenceEquals(node.Expressions[i], expressions[i]))
                changed = true;

            expressions[i] = (IExpression)node.Expressions[i].Transform(this);
        }

        if (!changed)
            return node;

        return new TupleExpression(null, expressions)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISemanticNode TransformTupleType(TupleType node)
        => node;

    public ISemanticNode TransformTypeAlias(AliasDeclaration node)
        => node;

    public ISemanticNode TransformType(TypeDeclaration node)
    {
        foreach (var constructor in node.Constructors)
            constructor.Transform(this);

        foreach (var method in node.Methods)
            method.Transform(this);

        foreach (var property in node.Properties)
            property.Transform(this);

        return node;
    }

    public ISemanticNode TransformTypeNode(Type node)
        => node;

    public ISemanticNode TransformUnaryExpression(UnaryExpression node)
    {
        var operand = (IExpression)node.Operand.Transform(this);
        if (ReferenceEquals(operand, node.Operand))
            return node;

        return new UnaryExpression(null, node.Kind, operand)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public ISemanticNode TransformUse(Use node)
        => node;

    public ISemanticNode TransformVariable(VariableDeclaration node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        if (ReferenceEquals(expression, node.Expression))
            return node;

        return new VariableDeclaration(null, node.Name, node.Type, expression);
    }

    public ISemanticNode TransformWhile(While node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);
        if (ReferenceEquals(condition, node.Condition))
            return node;

        return new While(null, condition, body);
    }
}