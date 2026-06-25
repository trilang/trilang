using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics;

// TODO: fix symbol table!!!
// TODO: create a full copy?
internal abstract class Transformer : ITransformer<ISemanticNode>
{
    protected readonly ISet<string> directives;
    protected readonly BuiltInTypes builtInTypes;

    protected Transformer(ISet<string> directives, BuiltInTypes builtInTypes)
    {
        this.directives = directives;
        this.builtInTypes = builtInTypes;
    }

    protected (bool, T[]) TransformNodes<T>(IReadOnlyList<T> nodes) where T : ISemanticNode
    {
        var changed = false;
        var updatedNodes = new T[nodes.Count];
        for (var i = 0; i < updatedNodes.Length; i++)
        {
            updatedNodes[i] = (T)nodes[i].Transform(this);

            if (!ReferenceEquals(nodes[i], updatedNodes[i]))
                changed = true;
        }

        return (changed, updatedNodes);
    }

    public virtual ISemanticNode TransformAlias(AliasDeclaration node)
    {
        var (isGenericArgumentsChanged, genericArguments) = TransformNodes(node.GenericArguments);
        var type = (IInlineType)node.Type.Transform(this);
        if (!isGenericArgumentsChanged && type == node.Type)
            return node;

        return new AliasDeclaration(node.SourceSpan, node.AccessModifier, node.Name, genericArguments, type)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformArrayAccess(ArrayAccessExpression node)
    {
        var member = (IAccessExpression)node.Member.Transform(this);
        var index = (IExpression)node.Index.Transform(this);
        if (member == node.Member && index == node.Index)
            return node;

        return new ArrayAccessExpression(node.SourceSpan, member, index)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
            Reference = node.Reference,
        };
    }

    public virtual ISemanticNode TransformArrayType(ArrayType node)
    {
        var inlineType = (IInlineType)node.ElementType.Transform(this);
        if (inlineType == node.ElementType)
            return node;

        return new ArrayType(node.SourceSpan, inlineType)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformBinaryExpression(BinaryExpression node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);
        if (left == node.Left && right == node.Right)
            return node;

        return new BinaryExpression(node.SourceSpan, node.Kind, left, right)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public virtual ISemanticNode TransformBlock(BlockStatement node)
    {
        var (isChanged, statements) = TransformNodes(node.Statements);
        if (!isChanged)
            return node;

        return new BlockStatement(node.SourceSpan, statements);
    }

    public virtual ISemanticNode TransformBreak(Break node)
        => node;

    public virtual ISemanticNode TransformCall(CallExpression node)
    {
        var member = (IAccessExpression)node.Member.Transform(this);
        var (isChanged, parameters) = TransformNodes(node.Parameters);
        if (member == node.Member && !isChanged)
            return node;

        return new CallExpression(node.SourceSpan, member, parameters);
    }

    public virtual ISemanticNode TransformCast(CastExpression node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var expression = (IExpression)node.Expression.Transform(this);
        if (type == node.Type && expression == node.Expression)
            return node;

        return new CastExpression(node.SourceSpan, type, expression);
    }

    public virtual ISemanticNode TransformConstructor(ConstructorDeclaration node)
    {
        var (isChanged, parameters) = TransformNodes(node.Parameters);
        var body = (BlockStatement)node.Body.Transform(this);
        if (!isChanged && body == node.Body)
            return node;

        return new ConstructorDeclaration(node.SourceSpan, node.AccessModifier, parameters, body)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformContinue(Continue node)
        => node;

    public virtual ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnion node)
    {
        var (isChanged, types) = TransformNodes(node.Types);
        if (!isChanged)
            return node;

        return new DiscriminatedUnion(node.SourceSpan, types)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformExpressionBlock(ExpressionBlock node)
    {
        var (isChanged, statements) = TransformNodes(node.Statements);
        if (!isChanged)
            return node;

        return new ExpressionBlock(statements);
    }

    public virtual ISemanticNode TransformExpressionStatement(ExpressionStatement node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        if (expression == node.Expression)
            return node;

        return new ExpressionStatement(node.SourceSpan, expression);
    }

    public virtual ISemanticNode TransformFakeDeclaration(FakeDeclaration node)
        => node;

    public virtual ISemanticNode TransformFakeExpression(FakeExpression node)
        => node;

    public virtual ISemanticNode TransformFakeStatement(FakeStatement node)
        => node;

    public virtual ISemanticNode TransformFakeType(FakeType node)
        => node;

    public virtual ISemanticNode TransformFunction(FunctionDeclaration node)
    {
        var (isChanged, parameters) = TransformNodes(node.Parameters);
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);
        if (!isChanged && parameters == node.Parameters && returnType == node.ReturnType && body == node.Body)
            return node;

        return new FunctionDeclaration(node.SourceSpan, node.AccessModifier, node.Name, parameters, returnType, body)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformFunctionType(FunctionType node)
    {
        var (isChanged, parameters) = TransformNodes(node.ParameterTypes);
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        if (!isChanged && parameters == node.ParameterTypes && returnType == node.ReturnType)
            return node;

        return new FunctionType(node.SourceSpan, parameters, returnType)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformGenericType(GenericApplication node)
    {
        var type = (TypeRef)node.Type.Transform(this);
        var (isChanged, genericArguments) = TransformNodes(node.TypeArguments);
        if (type == node.Type && !isChanged)
            return node;

        return new GenericApplication(node.SourceSpan, type, genericArguments)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformGenericExpression(GenericExpression node)
    {
        var member = (MemberAccessExpression)node.Member.Transform(this);
        var (isChanged, genericArguments) = TransformNodes(node.GenericArguments);
        if (member == node.Member && !isChanged)
            return node;

        return new GenericExpression(node.SourceSpan, member, genericArguments);
    }

    public virtual ISemanticNode TransformGoTo(GoTo node)
        => node;

    public virtual ISemanticNode TransformIfDirective(IfDirective node)
    {
        var condition = directives.Contains(node.DirectiveName);
        var (isThenChanged, thenStatements) = condition ? TransformNodes(node.Then) : (false, node.Then);
        var (isElseChanged, elseStatements) = !condition ? TransformNodes(node.Else) : (false, node.Else);
        if (!isThenChanged && !isElseChanged)
            return node;

        return new IfDirective(node.SourceSpan, node.DirectiveName, thenStatements, elseStatements);
    }

    public virtual ISemanticNode TransformIf(IfStatement node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var then = (BlockStatement)node.Then.Transform(this);
        var @else = (BlockStatement?)node.Else?.Transform(this);
        if (condition == node.Condition && then == node.Then && @else == node.Else)
            return node;

        return new IfStatement(node.SourceSpan, condition, then, @else);
    }

    public virtual ISemanticNode TransformInterface(Interface node)
    {
        var (isPropertiesChanged, properties) = TransformNodes(node.Properties);
        var (isMethodsChanged, methods) = TransformNodes(node.Methods);
        if (!isPropertiesChanged && !isMethodsChanged)
            return node;

        return new Interface(node.SourceSpan, properties, methods)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformInterfaceProperty(InterfaceProperty node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        if (type == node.Type)
            return node;

        return new InterfaceProperty(node.SourceSpan, node.Name, type, node.GetterModifier, node.SetterModifier)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformInterfaceMethod(InterfaceMethod node)
    {
        var (isChanged, parameters) = TransformNodes(node.ParameterTypes);
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        if (!isChanged && returnType == node.ReturnType)
            return node;

        return new InterfaceMethod(node.SourceSpan, node.Name, parameters, returnType)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformIsExpression(IsExpression node)
    {
        var expression = (IExpression)node.Expression.Transform(this);
        var type = (IInlineType)node.Type.Transform(this);
        if (expression == node.Expression && type == node.Type)
            return node;

        return new IsExpression(node.SourceSpan, expression, type, builtInTypes);
    }

    public virtual ISemanticNode TransformLabel(Label node)
        => node;

    public virtual ISemanticNode TransformLiteral(LiteralExpression node)
        => node;

    public virtual ISemanticNode TransformMemberAccess(MemberAccessExpression node)
    {
        var member = (IExpression?)node.Member?.Transform(this);
        if (member == node.Member)
            return node;

        return new MemberAccessExpression(node.SourceSpan, member, node.Name)
        {
            AccessKind = node.AccessKind,
            Reference = node.Reference,
        };
    }

    public virtual ISemanticNode TransformMethod(MethodDeclaration node)
    {
        var (isChanged, parameters) = TransformNodes(node.Parameters);
        var returnType = (IInlineType)node.ReturnType.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);
        if (!isChanged && returnType == node.ReturnType && body == node.Body)
            return node;

        return new MethodDeclaration(node.SourceSpan, node.AccessModifier, node.IsStatic, node.Name, parameters, returnType, body)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformNamespace(Namespace node)
        => node;

    public virtual ISemanticNode TransformNewObject(NewObjectExpression node)
    {
        var member = (IAccessExpression)node.Member.Transform(this);
        if (member == node.Member)
            return node;

        return new NewObjectExpression(node.SourceSpan, member)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformNull(NullExpression node)
        => node;

    public virtual ISemanticNode TransformParameter(Parameter node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        if (type == node.Type)
            return node;

        return new Parameter(node.SourceSpan, node.Name, type)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformPointer(PointerType node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        if (type == node.Type)
            return node;

        return new PointerType(node.SourceSpan, type)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformProperty(PropertyDeclaration node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var getter = (PropertyGetter?)node.Getter?.Transform(this);
        var setter = (PropertySetter?)node.Setter?.Transform(this);
        if (type == node.Type && getter == node.Getter && setter == node.Setter)
            return node;

        return new PropertyDeclaration(node.SourceSpan, node.Name, type, getter, setter)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformGetter(PropertyGetter node)
    {
        var body = (BlockStatement?)node.Body?.Transform(this);
        if (body == node.Body)
            return node;

        return new PropertyGetter(node.SourceSpan, node.AccessModifier, body)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformSetter(PropertySetter node)
    {
        var body = (BlockStatement?)node.Body?.Transform(this);
        if (body == node.Body)
            return node;

        return new PropertySetter(node.SourceSpan, node.AccessModifier, body)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformReturn(ReturnStatement node)
    {
        var expression = (IExpression?)node.Expression?.Transform(this);
        if (expression == node.Expression)
            return node;

        return new ReturnStatement(node.SourceSpan, expression);
    }

    public virtual ISemanticNode TransformTree(SemanticTree node)
    {
        var ns = (Namespace)node.Namespace.Transform(this);
        var (isUsesChanged, uses) = TransformNodes(node.UseNodes);
        var (isDeclarationsChanged, declarations) = TransformNodes(node.Declarations);
        if (node.Namespace == ns && !isUsesChanged && !isDeclarationsChanged)
            return node;

        return new SemanticTree(node.SourceFile, node.SourceSpan, ns, uses, declarations);
    }

    public virtual ISemanticNode TransformTuple(TupleExpression node)
    {
        var (isChanged, expressions) = TransformNodes(node.Expressions);
        if (!isChanged)
            return node;

        return new TupleExpression(node.SourceSpan, expressions)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public virtual ISemanticNode TransformTupleType(TupleType node)
    {
        var (isChanged, types) = TransformNodes(node.Types);
        if (!isChanged)
            return node;

        return new TupleType(node.SourceSpan, types)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformType(TypeDeclaration node)
    {
        var (isGenericArgumentsChanged, genericArguments) = TransformNodes(node.GenericArguments);
        var (isInterfacesChanged, interfaces) = TransformNodes(node.Interfaces);
        var (isPropertiesChanged, properties) = TransformNodes(node.Properties);
        var (isConstructorsChanged, constructors) = TransformNodes(node.Constructors);
        var (isMethodsChanged, methods) = TransformNodes(node.Methods);
        if (!isGenericArgumentsChanged && !isInterfacesChanged && !isPropertiesChanged && !isConstructorsChanged && !isMethodsChanged)
            return node;

        return new TypeDeclaration(node.SourceSpan, node.AccessModifier, node.Name, genericArguments, interfaces, properties, constructors, methods)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformTypeNode(TypeRef node)
        => node;

    public virtual ISemanticNode TransformUnaryExpression(UnaryExpression node)
    {
        var operand = (IExpression)node.Operand.Transform(this);
        if (operand == node.Operand)
            return node;

        return new UnaryExpression(node.SourceSpan, node.Kind, operand)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public virtual ISemanticNode TransformUse(Use node)
        => node;

    public virtual ISemanticNode TransformVariable(VariableDeclaration node)
    {
        var type = (IInlineType)node.Type.Transform(this);
        var expression = (IExpression)node.Expression.Transform(this);
        if (type == node.Type && expression == node.Expression)
            return node;

        return new VariableDeclaration(node.SourceSpan, node.Name, type, expression)
        {
            Metadata = node.Metadata,
        };
    }

    public virtual ISemanticNode TransformWhile(While node)
    {
        var condition = (IExpression)node.Condition.Transform(this);
        var body = (BlockStatement)node.Body.Transform(this);
        if (condition == node.Condition && body == node.Body)
            return node;

        var loop = new While(node.SourceSpan, condition, body);
        var breakNodes = body.Where<Break>();
        foreach (var breakNode in breakNodes)
            if (breakNode.LoopNode == node)
                breakNode.LoopNode = loop;

        var continueNodes = body.Where<Continue>();
        foreach (var continueNode in continueNodes)
            if (continueNode.LoopNode == node)
                continueNode.LoopNode = loop;

        return loop;
    }
}