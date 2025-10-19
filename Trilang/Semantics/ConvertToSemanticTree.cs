using Trilang.Parsing;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics;

internal class ConvertToSemanticTree : INodeTransformer<ISemanticNode>
{
    public ISemanticNode TransformArrayAccess(Parsing.Ast.ArrayAccessExpressionNode node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var index = (IExpression)node.Index.Transform(this);

        return new ArrayAccessExpression(node.SourceSpan, member, index);
    }

    public ISemanticNode TransformArrayType(Parsing.Ast.ArrayTypeNode node)
    {
        var elementType = (IInlineType)node.ElementType.Transform(this);

        return new ArrayType(node.SourceSpan, elementType);
    }

    public ISemanticNode TransformBinaryExpression(Parsing.Ast.BinaryExpressionNode node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);
        var kind = (BinaryExpressionKind)node.Kind;

        return new BinaryExpression(node.SourceSpan, kind, left, right);
    }

    public ISemanticNode TransformBlock(Parsing.Ast.BlockStatementNode node)
    {
        var statements = node.Statements.Select(s => (IStatement)s.Transform(this)).ToList();

        return new BlockStatement(node.SourceSpan, statements);
    }

    public ISemanticNode TransformBreak(Parsing.Ast.BreakNode node)
        => new Break(node.SourceSpan);

    public ISemanticNode TransformCall(Parsing.Ast.CallExpressionNode node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new CallExpression(node.SourceSpan, member, arguments);
    }

    public ISemanticNode TransformCast(Parsing.Ast.CastExpressionNode node)
    {
        var targetType = (IInlineType)node.Type.Transform(this);
        var expression = (IExpression)node.Expression.Transform(this);

        return new CastExpression(node.SourceSpan, targetType, expression);
    }

    public ISemanticNode TransformConstructor(Parsing.Ast.ConstructorDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var body = (BlockStatement)node.Body.Transform(this);

        return new ConstructorDeclaration(node.SourceSpan, accessModifier, parameters, body);
    }

    public ISemanticNode TransformContinue(Parsing.Ast.ContinueNode node)
        => new Continue(node.SourceSpan);

    public ISemanticNode TransformDiscriminatedUnion(Parsing.Ast.DiscriminatedUnionNode node)
    {
        var types = node.Types.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new DiscriminatedUnion(node.SourceSpan, types);
    }

    public ISemanticNode TransformExpressionStatement(Parsing.Ast.ExpressionStatementNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);

        return new ExpressionStatement(node.SourceSpan, expression);
    }

    public ISemanticNode TransformFakeDeclaration(Parsing.Ast.FakeDeclarationNode node)
        => new FakeDeclaration(node.SourceSpan);

    public ISemanticNode TransformFakeExpression(Parsing.Ast.FakeExpressionNode node)
        => new FakeExpression(node.SourceSpan);

    public ISemanticNode TransformFakeStatement(Parsing.Ast.FakeStatementNode node)
        => new FakeStatement(node.SourceSpan);

    public ISemanticNode TransformFakeType(Parsing.Ast.FakeTypeNode node)
        => new FakeType(node.SourceSpan, node.Name);

    public ISemanticNode TransformFunction(Parsing.Ast.FunctionDeclarationNode node)
    {
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new FunctionDeclaration(node.SourceSpan, (AccessModifier)node.AccessModifier, node.Name, parameters, returnType, body);
    }

    public ISemanticNode TransformFunctionType(Parsing.Ast.FunctionTypeNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new FunctionType(node.SourceSpan, parameters, returnType);
    }

    public ISemanticNode TransformGenericType(Parsing.Ast.GenericTypeNode node)
    {
        var typeArguments = node.TypeArguments.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new GenericType(node.SourceSpan, node.PrefixName, typeArguments);
    }

    public ISemanticNode TransformIfDirective(Parsing.Ast.IfDirectiveNode node)
    {
        var statements = node.Then.Select(s => s.Transform(this)).ToList();
        var elseBranch = node.Else.Select(s => s.Transform(this)).ToList();

        return new IfDirective(node.SourceSpan, node.DirectiveName, statements, elseBranch);
    }

    public ISemanticNode TransformIf(Parsing.Ast.IfStatementNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var thenBranch = (BlockStatement)node.Then.Transform(this);
        var elseBranch = (BlockStatement?)node.Else?.Transform(this);

        return new IfStatement(node.SourceSpan, condition, thenBranch, elseBranch);
    }

    public ISemanticNode TransformInterface(Parsing.Ast.InterfaceNode node)
    {
        var properties = node.Properties.Select(p => (InterfaceProperty)p.Transform(this)).ToList();
        var methods = node.Methods.Select(m => (InterfaceMethod)m.Transform(this)).ToList();

        return new Interface(node.SourceSpan, properties, methods);
    }

    public ISemanticNode TransformInterfaceProperty(Parsing.Ast.InterfacePropertyNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (AccessModifier?)node.GetterModifier;
        var setter = (AccessModifier?)node.SetterModifier;

        return new InterfaceProperty(node.SourceSpan, node.Name, type, getter, setter);
    }

    public ISemanticNode TransformInterfaceMethod(Parsing.Ast.InterfaceMethodNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new InterfaceMethod(node.SourceSpan, node.Name, parameters, returnType);
    }

    public ISemanticNode TransformAsExpression(Parsing.Ast.IsExpressionNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        var type = (IInlineType)node.Type.Transform(this);

        return new IsExpression(node.SourceSpan, expression, type);
    }

    public ISemanticNode TransformLiteral(Parsing.Ast.LiteralExpressionNode node)
    {
        var kind = node.Kind switch
        {
            Parsing.Ast.LiteralExpressionKind.Unknown => LiteralExpressionKind.Unknown,
            Parsing.Ast.LiteralExpressionKind.Integer => LiteralExpressionKind.Integer,
            Parsing.Ast.LiteralExpressionKind.Float => LiteralExpressionKind.Float,
            Parsing.Ast.LiteralExpressionKind.Boolean => LiteralExpressionKind.Boolean,
            Parsing.Ast.LiteralExpressionKind.String => LiteralExpressionKind.String,
            Parsing.Ast.LiteralExpressionKind.Char => LiteralExpressionKind.Char,

            _ => throw new ArgumentOutOfRangeException(),
        };

        return new LiteralExpression(node.SourceSpan, kind, node.Value);
    }

    public ISemanticNode TransformMemberAccess(Parsing.Ast.MemberAccessExpressionNode node)
    {
        var member = (IExpression?)node.Member?.Transform(this);

        return new MemberAccessExpression(node.SourceSpan, member, node.Name);
    }

    public ISemanticNode TransformMethod(Parsing.Ast.MethodDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new MethodDeclaration(
            node.SourceSpan,
            accessModifier,
            node.IsStatic,
            node.Name,
            parameters,
            returnType,
            body);
    }

    public ISemanticNode TransformNewArray(Parsing.Ast.NewArrayExpressionNode node)
    {
        var type = (ArrayType)node.Type.Transform(this);
        var size = (IExpression)node.Size.Transform(this);

        return new NewArrayExpression(node.SourceSpan, type, size);
    }

    public ISemanticNode TransformNewObject(Parsing.Ast.NewObjectExpressionNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new NewObjectExpression(node.SourceSpan, type, arguments);
    }

    public ISemanticNode TransformNull(Parsing.Ast.NullExpressionNode node)
        => new NullExpression(node.SourceSpan);

    public ISemanticNode TransformParameter(Parsing.Ast.ParameterNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);

        return new Parameter(node.SourceSpan, node.Name, type);
    }

    public ISemanticNode TransformProperty(Parsing.Ast.PropertyDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (PropertyGetter?)node.Getter?.Transform(this);
        var setter = (PropertySetter?)node.Setter?.Transform(this);

        return new PropertyDeclaration(node.SourceSpan, node.Name, type, getter, setter);
    }

    public ISemanticNode TransformGetter(Parsing.Ast.PropertyGetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertyGetter(node.SourceSpan, accessModifier, body);
    }

    public ISemanticNode TransformSetter(Parsing.Ast.PropertySetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertySetter(node.SourceSpan, accessModifier, body);
    }

    public ISemanticNode TransformReturn(Parsing.Ast.ReturnStatementNode node)
    {
        var expression = (IExpression?)node.Expression?.Transform(this);

        return new ReturnStatement(node.SourceSpan, expression);
    }

    public ISemanticNode TransformTree(Parsing.Ast.SyntaxTree node)
    {
        var declarations = node.Declarations.Select(d => (IDeclaration)d.Transform(this)).ToList();

        return new SemanticTree(node.SourceFile, node.SourceSpan, declarations);
    }

    public ISemanticNode TransformTuple(Parsing.Ast.TupleExpressionNode node)
    {
        var elements = node.Expressions.Select(e => (IExpression)e.Transform(this)).ToList();

        return new TupleExpression(node.SourceSpan, elements);
    }

    public ISemanticNode TransformTupleType(Parsing.Ast.TupleTypeNode node)
    {
        var types = node.Types.Select(e => (IInlineType)e.Transform(this)).ToList();

        return new TupleType(node.SourceSpan, types);
    }

    public ISemanticNode TransformTypeAlias(Parsing.Ast.TypeAliasDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (Type)ga.Transform(this)).ToList();
        var type = (IInlineType)node.Type.Transform(this);

        return new TypeAliasDeclaration(node.SourceSpan, accessModifier, node.Name, genericArguments, type);
    }

    public ISemanticNode TransformType(Parsing.Ast.TypeDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (Type)ga.Transform(this)).ToList();
        var interfaces = node.Interfaces.Select(i => (Type)i.Transform(this)).ToList();
        var properties = node.Properties.Select(f => (PropertyDeclaration)f.Transform(this)).ToList();
        var constructors = node.Constructors.Select(c => (ConstructorDeclaration)c.Transform(this)).ToList();
        var methods = node.Methods.Select(m => (MethodDeclaration)m.Transform(this)).ToList();

        return new TypeDeclaration(
            node.SourceSpan,
            accessModifier,
            node.Name,
            genericArguments,
            interfaces,
            properties,
            constructors,
            methods);
    }

    public ISemanticNode TransformTypeNode(Parsing.Ast.TypeNode node)
        => new Type(node.SourceSpan, node.Name);

    public ISemanticNode TransformUnaryExpression(Parsing.Ast.UnaryExpressionNode node)
    {
        var operand = (IExpression)node.Operand.Transform(this);
        var kind = (UnaryExpressionKind)node.Kind;

        return new UnaryExpression(node.SourceSpan, kind, operand);
    }

    public ISemanticNode TransformVariable(Parsing.Ast.VariableDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var initializer = (IExpression)node.Expression.Transform(this);

        return new VariableDeclaration(node.SourceSpan, node.Name, type, initializer);
    }

    public ISemanticNode TransformWhile(Parsing.Ast.WhileNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new While(node.SourceSpan, condition, body);
    }
}