using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics.Model;
using AccessModifier = Trilang.Semantics.Model.AccessModifier;
using BinaryExpressionKind = Trilang.Semantics.Model.BinaryExpressionKind;
using LiteralExpressionKind = Trilang.Semantics.Model.LiteralExpressionKind;
using UnaryExpressionKind = Trilang.Semantics.Model.UnaryExpressionKind;

namespace Trilang.Semantics;

internal class ConvertToSemanticTree : INodeTransformer<ISemanticNode>
{
    public ISemanticNode TransformTypeAlias(AliasDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (TypeRef)ga.Transform(this)).ToList();
        var type = (IInlineType)node.Type.Transform(this);

        return new AliasDeclaration(node.SourceSpan, accessModifier, node.Name, genericArguments, type);
    }

    public ISemanticNode TransformArrayAccess(ArrayAccessExpressionNode node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var index = (IExpression)node.Index.Transform(this);

        return new ArrayAccessExpression(node.SourceSpan, member, index);
    }

    public ISemanticNode TransformArrayType(ArrayTypeNode node)
    {
        var elementType = (IInlineType)node.ElementType.Transform(this);

        return new ArrayType(node.SourceSpan, elementType);
    }

    public ISemanticNode TransformBinaryExpression(BinaryExpressionNode node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);
        var kind = (BinaryExpressionKind)node.Kind;

        return new BinaryExpression(node.SourceSpan, kind, left, right);
    }

    public ISemanticNode TransformBlock(BlockStatementNode node)
    {
        var statements = node.Statements.Select(s => (IStatement)s.Transform(this)).ToList();

        return new BlockStatement(node.SourceSpan, statements);
    }

    public ISemanticNode TransformBreak(BreakNode node)
        => new Break(node.SourceSpan);

    public ISemanticNode TransformCall(CallExpressionNode node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new CallExpression(node.SourceSpan, member, arguments);
    }

    public ISemanticNode TransformCast(CastExpressionNode node)
    {
        var targetType = (IInlineType)node.Type.Transform(this);
        var expression = (IExpression)node.Expression.Transform(this);

        return new CastExpression(node.SourceSpan, targetType, expression);
    }

    public ISemanticNode TransformConstructor(ConstructorDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var body = (BlockStatement)node.Body.Transform(this);

        return new ConstructorDeclaration(node.SourceSpan, accessModifier, parameters, body);
    }

    public ISemanticNode TransformContinue(ContinueNode node)
        => new Continue(node.SourceSpan);

    public ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnionNode node)
    {
        var types = node.Types.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new DiscriminatedUnion(node.SourceSpan, types);
    }

    public ISemanticNode TransformExpressionStatement(ExpressionStatementNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);

        return new ExpressionStatement(node.SourceSpan, expression);
    }

    public ISemanticNode TransformFakeDeclaration(FakeDeclarationNode node)
        => new FakeDeclaration(node.SourceSpan);

    public ISemanticNode TransformFakeExpression(FakeExpressionNode node)
        => new FakeExpression(node.SourceSpan);

    public ISemanticNode TransformFakeStatement(FakeStatementNode node)
        => new FakeStatement(node.SourceSpan);

    public ISemanticNode TransformFakeType(FakeTypeNode node)
        => new FakeType(node.SourceSpan, node.Name);

    public ISemanticNode TransformFunction(FunctionDeclarationNode node)
    {
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new FunctionDeclaration(node.SourceSpan, (AccessModifier)node.AccessModifier, node.Name, parameters, returnType, body);
    }

    public ISemanticNode TransformFunctionType(FunctionTypeNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new FunctionType(node.SourceSpan, parameters, returnType);
    }

    public ISemanticNode TransformGenericType(GenericApplicationNode node)
    {
        var typeArguments = node.TypeArguments.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new GenericApplication(node.SourceSpan, node.PrefixName, typeArguments);
    }

    public ISemanticNode TransformIfDirective(IfDirectiveNode node)
    {
        var statements = node.Then.Select(s => s.Transform(this)).ToList();
        var elseBranch = node.Else.Select(s => s.Transform(this)).ToList();

        return new IfDirective(node.SourceSpan, node.DirectiveName, statements, elseBranch);
    }

    public ISemanticNode TransformIf(IfStatementNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var thenBranch = (BlockStatement)node.Then.Transform(this);
        var elseBranch = (BlockStatement?)node.Else?.Transform(this);

        return new IfStatement(node.SourceSpan, condition, thenBranch, elseBranch);
    }

    public ISemanticNode TransformInterface(InterfaceNode node)
    {
        var properties = node.Properties.Select(p => (InterfaceProperty)p.Transform(this)).ToList();
        var methods = node.Methods.Select(m => (InterfaceMethod)m.Transform(this)).ToList();

        return new Interface(node.SourceSpan, properties, methods);
    }

    public ISemanticNode TransformInterfaceProperty(InterfacePropertyNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (AccessModifier?)node.GetterModifier;
        var setter = (AccessModifier?)node.SetterModifier;

        return new InterfaceProperty(node.SourceSpan, node.Name, type, getter, setter);
    }

    public ISemanticNode TransformInterfaceMethod(InterfaceMethodNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new InterfaceMethod(node.SourceSpan, node.Name, parameters, returnType);
    }

    public ISemanticNode TransformAsExpression(IsExpressionNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        var type = (IInlineType)node.Type.Transform(this);

        return new IsExpression(node.SourceSpan, expression, type);
    }

    public ISemanticNode TransformLiteral(LiteralExpressionNode node)
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

    public ISemanticNode TransformMemberAccess(MemberAccessExpressionNode node)
    {
        var member = (IExpression?)node.Member?.Transform(this);

        return new MemberAccessExpression(node.SourceSpan, member, node.Name);
    }

    public ISemanticNode TransformMethod(MethodDeclarationNode node)
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

    public ISemanticNode TransformNamespace(NamespaceNode node)
        => new Namespace(node.SourceSpan, node.Parts);

    public ISemanticNode TransformNewArray(NewArrayExpressionNode node)
    {
        var type = (ArrayType)node.Type.Transform(this);
        var size = (IExpression)node.Size.Transform(this);

        return new NewArrayExpression(node.SourceSpan, type, size);
    }

    public ISemanticNode TransformNewObject(NewObjectExpressionNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new NewObjectExpression(node.SourceSpan, type, arguments);
    }

    public ISemanticNode TransformNull(NullExpressionNode node)
        => new NullExpression(node.SourceSpan);

    public ISemanticNode TransformParameter(ParameterNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);

        return new Parameter(node.SourceSpan, node.Name, type);
    }

    public ISemanticNode TransformProperty(PropertyDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (PropertyGetter?)node.Getter?.Transform(this);
        var setter = (PropertySetter?)node.Setter?.Transform(this);

        return new PropertyDeclaration(node.SourceSpan, node.Name, type, getter, setter);
    }

    public ISemanticNode TransformGetter(PropertyGetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertyGetter(node.SourceSpan, accessModifier, body);
    }

    public ISemanticNode TransformSetter(PropertySetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertySetter(node.SourceSpan, accessModifier, body);
    }

    public ISemanticNode TransformReturn(ReturnStatementNode node)
    {
        var expression = (IExpression?)node.Expression?.Transform(this);

        return new ReturnStatement(node.SourceSpan, expression);
    }

    public ISemanticNode TransformTree(SyntaxTree node)
    {
        var declarations = node.Declarations.Select(d => (IDeclaration)d.Transform(this)).ToList();

        return new SemanticTree(
            node.SourceFile,
            node.SourceSpan,
            node.UseNodes.Select(u => (Use)u.Transform(this)).ToList(),
            (Namespace?)node.Namespace?.Transform(this),
            declarations);
    }

    public ISemanticNode TransformTuple(TupleExpressionNode node)
    {
        var elements = node.Expressions.Select(e => (IExpression)e.Transform(this)).ToList();

        return new TupleExpression(node.SourceSpan, elements);
    }

    public ISemanticNode TransformTupleType(TupleTypeNode node)
    {
        var types = node.Types.Select(e => (IInlineType)e.Transform(this)).ToList();

        return new TupleType(node.SourceSpan, types);
    }

    public ISemanticNode TransformType(TypeDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (TypeRef)ga.Transform(this)).ToList();
        var interfaces = node.Interfaces.Select(i => (TypeRef)i.Transform(this)).ToList();
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

    public ISemanticNode TransformTypeNode(TypeRefNode node)
        => new TypeRef(node.SourceSpan, node.Name);

    public ISemanticNode TransformUnaryExpression(UnaryExpressionNode node)
    {
        var operand = (IExpression)node.Operand.Transform(this);
        var kind = (UnaryExpressionKind)node.Kind;

        return new UnaryExpression(node.SourceSpan, kind, operand);
    }

    public ISemanticNode TransformUse(UseNode node)
        => new Use(node.SourceSpan, node.Parts);

    public ISemanticNode TransformVariable(VariableDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var initializer = (IExpression)node.Expression.Transform(this);

        return new VariableDeclaration(node.SourceSpan, node.Name, type, initializer);
    }

    public ISemanticNode TransformWhile(WhileNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new While(node.SourceSpan, condition, body);
    }
}