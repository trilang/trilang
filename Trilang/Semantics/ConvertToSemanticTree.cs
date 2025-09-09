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

        return new ArrayAccessExpression(member, index);
    }

    public ISemanticNode TransformArrayType(Parsing.Ast.ArrayTypeNode node)
    {
        var elementType = (IInlineType)node.ElementType.Transform(this);

        return new ArrayType(elementType);
    }

    public ISemanticNode TransformBinaryExpression(Parsing.Ast.BinaryExpressionNode node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);
        var kind = (BinaryExpressionKind)node.Kind;

        return new BinaryExpression(kind, left, right);
    }

    public ISemanticNode TransformBlock(Parsing.Ast.BlockStatementNode node)
    {
        var statements = node.Statements.Select(s => (IStatement)s.Transform(this)).ToList();

        return new BlockStatement(statements);
    }

    public ISemanticNode TransformBreak(Parsing.Ast.BreakNode node)
        => new Break();

    public ISemanticNode TransformCall(Parsing.Ast.CallExpressionNode node)
    {
        var member = (IExpression)node.Member.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new CallExpression(member, arguments);
    }

    public ISemanticNode TransformCast(Parsing.Ast.CastExpressionNode node)
    {
        var targetType = (IInlineType)node.Type.Transform(this);
        var expression = (IExpression)node.Expression.Transform(this);

        return new CastExpression(targetType, expression);
    }

    public ISemanticNode TransformConstructor(Parsing.Ast.ConstructorDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var body = (BlockStatement)node.Body.Transform(this);

        return new ConstructorDeclaration(accessModifier, parameters, body);
    }

    public ISemanticNode TransformContinue(Parsing.Ast.ContinueNode node)
        => new Continue();

    public ISemanticNode TransformDiscriminatedUnion(Parsing.Ast.DiscriminatedUnionNode node)
    {
        var types = node.Types.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new DiscriminatedUnion(types);
    }

    public ISemanticNode TransformExpressionStatement(Parsing.Ast.ExpressionStatementNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);

        return new ExpressionStatement(expression);
    }

    public ISemanticNode TransformFunction(Parsing.Ast.FunctionDeclarationNode node)
    {
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new FunctionDeclaration(node.Name, parameters, returnType, body);
    }

    public ISemanticNode TransformFunctionType(Parsing.Ast.FunctionTypeNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new FunctionType(parameters, returnType);
    }

    public ISemanticNode TransformGenericType(Parsing.Ast.GenericTypeNode node)
    {
        var typeArguments = node.TypeArguments.Select(t => (IInlineType)t.Transform(this)).ToList();

        return new GenericType(node.PrefixName, typeArguments);
    }

    public ISemanticNode TransformIfDirective(Parsing.Ast.IfDirectiveNode node)
    {
        var statements = node.Then.Select(s => s.Transform(this)).ToList();
        var elseBranch = node.Else.Select(s => s.Transform(this)).ToList();

        return new IfDirective(node.DirectiveName, statements, elseBranch);
    }

    public ISemanticNode TransformIf(Parsing.Ast.IfStatementNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var thenBranch = (BlockStatement)node.Then.Transform(this);
        var elseBranch = (BlockStatement?)node.Else?.Transform(this);

        return new IfStatement(condition, thenBranch, elseBranch);
    }

    public ISemanticNode TransformInterface(Parsing.Ast.InterfaceNode node)
    {
        var properties = node.Properties.Select(p => (InterfaceProperty)p.Transform(this)).ToList();
        var methods = node.Methods.Select(m => (InterfaceMethod)m.Transform(this)).ToList();

        return new Interface(properties, methods);
    }

    public ISemanticNode TransformInterfaceProperty(Parsing.Ast.InterfacePropertyNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (AccessModifier?)node.GetterModifier;
        var setter = (AccessModifier?)node.GetterModifier;

        return new InterfaceProperty(node.Name, type, getter, setter);
    }

    public ISemanticNode TransformInterfaceMethod(Parsing.Ast.InterfaceMethodNode node)
    {
        var parameters = node.ParameterTypes.Select(p => (IInlineType)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);

        return new InterfaceMethod(node.Name, parameters, returnType);
    }

    public ISemanticNode TransformAsExpression(Parsing.Ast.IsExpressionNode node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        var type = (IInlineType)node.Type.Transform(this);

        return new IsExpression(expression, type);
    }

    public ISemanticNode TransformLiteral(Parsing.Ast.LiteralExpressionNode node)
        => new LiteralExpression((LiteralExpressionKind)node.Kind, node.Value);

    public ISemanticNode TransformMemberAccess(Parsing.Ast.MemberAccessExpressionNode node)
    {
        var member = (IExpression?)node.Member?.Transform(this);

        return new MemberAccessExpression(member, node.Name);
    }

    public ISemanticNode TransformMethod(Parsing.Ast.MethodDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var parameters = node.Parameters.Select(p => (Parameter)p.Transform(this)).ToList();
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new MethodDeclaration(accessModifier, node.IsStatic, node.Name, parameters, returnType, body);
    }

    public ISemanticNode TransformNewArray(Parsing.Ast.NewArrayExpressionNode node)
    {
        var type = (ArrayType)node.Type.Transform(this);
        var size = (IExpression)node.Size.Transform(this);

        return new NewArrayExpression(type, size);
    }

    public ISemanticNode TransformNewObject(Parsing.Ast.NewObjectExpressionNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var arguments = node.Parameters.Select(a => (IExpression)a.Transform(this)).ToList();

        return new NewObjectExpression(type, arguments);
    }

    public ISemanticNode TransformNull(Parsing.Ast.NullExpressionNode node)
        => new NullExpression();

    public ISemanticNode TransformParameter(Parsing.Ast.ParameterNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);

        return new Parameter(node.Name, type);
    }

    public ISemanticNode TransformProperty(Parsing.Ast.PropertyDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (PropertyGetter?)node.Getter?.Transform(this);
        var setter = (PropertySetter?)node.Setter?.Transform(this);

        return new PropertyDeclaration(node.Name, type, getter, setter);
    }

    public ISemanticNode TransformGetter(Parsing.Ast.PropertyGetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertyGetter(accessModifier, body);
    }

    public ISemanticNode TransformSetter(Parsing.Ast.PropertySetterNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var body = (BlockStatement?)node.Body?.Transform(this);

        return new PropertySetter(accessModifier, body);
    }

    public ISemanticNode TransformReturn(Parsing.Ast.ReturnStatementNode node)
    {
        var expression = (IExpression?)node.Expression?.Transform(this);

        return new ReturnStatement(expression);
    }

    public ISemanticNode TransformTree(Parsing.Ast.SyntaxTree node)
    {
        var declarations = node.Declarations.Select(d => (IDeclaration)d.Transform(this)).ToList();

        return new SemanticTree(declarations);
    }

    public ISemanticNode TransformTuple(Parsing.Ast.TupleExpressionNode node)
    {
        var elements = node.Expressions.Select(e => (IExpression)e.Transform(this)).ToList();

        return new TupleExpression(elements);
    }

    public ISemanticNode TransformTupleType(Parsing.Ast.TupleTypeNode node)
    {
        var types = node.Types.Select(e => (IInlineType)e.Transform(this)).ToList();

        return new TupleType(types);
    }

    public ISemanticNode TransformTypeAlias(Parsing.Ast.TypeAliasDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (Type)ga.Transform(this)).ToList();
        var type = (IInlineType)node.Type.Transform(this);

        return new TypeAliasDeclaration(accessModifier, node.Name, genericArguments, type);
    }

    public ISemanticNode TransformType(Parsing.Ast.TypeDeclarationNode node)
    {
        var accessModifier = (AccessModifier)node.AccessModifier;
        var genericArguments = node.GenericArguments.Select(ga => (Type)ga.Transform(this)).ToList();
        var interfaces = node.Interfaces.Select(i => (Type)i.Transform(this)).ToList();
        var properties = node.Properties.Select(f => (PropertyDeclaration)f.Transform(this)).ToList();
        var constructors = node.Constructors.Select(c => (ConstructorDeclaration)c.Transform(this)).ToList();
        var methods = node.Methods.Select(m => (MethodDeclaration)m.Transform(this)).ToList();

        return new TypeDeclaration(accessModifier, node.Name, genericArguments, interfaces, properties, constructors, methods);
    }

    public ISemanticNode TransformTypeNode(Parsing.Ast.TypeNode node)
        => new Type(node.Name);

    public ISemanticNode TransformUnaryExpression(Parsing.Ast.UnaryExpressionNode node)
    {
        var operand = (IExpression)node.Operand.Transform(this);
        var kind = (UnaryExpressionKind)node.Kind;

        return new UnaryExpression(kind, operand);
    }

    public ISemanticNode TransformVariable(Parsing.Ast.VariableDeclarationNode node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var initializer = (IExpression)node.Expression.Transform(this);

        return new VariableDeclaration(node.Name, type, initializer);
    }

    public ISemanticNode TransformWhile(Parsing.Ast.WhileNode node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);

        return new While(condition, body);
    }
}