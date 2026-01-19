using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class MetadataProviderAnalyzer : IVisitor<IMetadataProvider>, ISemanticPass
{
    private readonly MetadataProviderMap map;
    private HashSet<string> directives = null!;

    public MetadataProviderAnalyzer()
        => map = new MetadataProviderMap();

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        directives = context.Directives;

        foreach (var tree in semanticTrees)
            tree.Accept(this, context.RootMetadataProvider);

        context.MetadataProviderMap = map;
    }

    public void VisitAlias(AliasDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        var child = context.CreateChild();

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this, child);

        node.Type.Accept(this, child);
    }

    public void VisitArrayAccess(ArrayAccessExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

    public void VisitArrayType(ArrayType node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.ElementType.Accept(this, context);
    }

    public void VisitBinaryExpression(BinaryExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    public void VisitBlock(BlockStatement node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);
    }

    public void VisitBreak(Break node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitCall(CallExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitCast(CastExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);
    }

    public void VisitConstructor(ConstructorDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.Body.Accept(this, context);
    }

    public void VisitContinue(Continue node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitDiscriminatedUnion(DiscriminatedUnion node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);
    }

    public void VisitExpressionBlock(ExpressionBlock node, IMetadataProvider context)
        => Debug.Fail("Expression blocks are the compiler's internal feature and are not directly supported in the programming language.");

    public void VisitExpressionStatement(ExpressionStatement node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
    }

    public void VisitFakeDeclaration(FakeDeclaration node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitFakeExpression(FakeExpression node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitFakeStatement(FakeStatement node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitFakeType(FakeType node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitFunction(FunctionDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body.Accept(this, context);
    }

    public void VisitFunctionType(FunctionType node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void VisitGenericType(GenericTypeRef node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);
    }

    public void VisitGoTo(GoTo node, IMetadataProvider context)
        => Debug.Fail("`goto` is the compiler's internal feature and are not directly supported in the programming language.");

    public void VisitIfDirective(IfDirective node, IMetadataProvider context)
    {
        if (directives.Contains(node.DirectiveName))
        {
            foreach (var then in node.Then)
                then.Accept(this, context);
        }
        else
        {
            foreach (var @else in node.Else)
                @else.Accept(this, context);
        }
    }

    public void VisitIf(IfStatement node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void VisitInterface(Interface node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);
    }

    public void VisitInterfaceProperty(InterfaceProperty node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
    }

    public void VisitInterfaceMethod(InterfaceMethod node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void VisitIsExpression(IsExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);
    }

    public void VisitLabel(Label node, IMetadataProvider context)
        => Debug.Fail("Labels are the compiler's internal feature and are not directly supported in the programming language.");

    public void VisitLiteral(LiteralExpression node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitMemberAccess(MemberAccessExpression node, IMetadataProvider context)
    {
        node.Member?.Accept(this, context);

        map.Add(node, context);
    }

    public void VisitMethod(MethodDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body.Accept(this, context);
    }

    public void VisitNamespace(Namespace node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitNewArray(NewArrayExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);
    }

    public void VisitNewObject(NewObjectExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitNull(NullExpression node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitReturn(ReturnStatement node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Expression?.Accept(this, context);
    }

    public void VisitParameter(Parameter node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
    }

    public void VisitProperty(PropertyDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void VisitGetter(PropertyGetter node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Body?.Accept(this, context);
    }

    public void VisitSetter(PropertySetter node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Body?.Accept(this, context);
    }

    public void VisitTree(SemanticTree node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var declaration in node.Declarations)
            declaration.Accept(this, context);
    }

    public void VisitTuple(TupleExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var expression in node.Expressions)
            expression.Accept(this, context);
    }

    public void VisitTupleType(TupleType node, IMetadataProvider context)
    {
        map.Add(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);
    }

    public void VisitType(TypeDeclaration node, IMetadataProvider context)
    {
        var child = context.CreateChild();

        map.Add(node, child);

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this, child);

        foreach (var @interface in node.Interfaces)
            @interface.Accept(this, child);

        foreach (var property in node.Properties)
            property.Accept(this, child);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this, child);

        foreach (var method in node.Methods)
            method.Accept(this, child);
    }

    public void VisitTypeNode(TypeRef node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitUnaryExpression(UnaryExpression node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Operand.Accept(this, context);
    }

    public void VisitUse(Use node, IMetadataProvider context)
        => map.Add(node, context);

    public void VisitVariable(VariableDeclaration node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);
    }

    public void VisitWhile(While node, IMetadataProvider context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }

    public string Name => nameof(MetadataProviderAnalyzer);

    public IEnumerable<string> DependsOn => [];
}