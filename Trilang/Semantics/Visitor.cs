using Trilang.Semantics.Model;

namespace Trilang.Semantics;

internal abstract class Visitor : IVisitor
{
    protected readonly ISet<string> directives;

    protected Visitor(ISet<string> directives)
        => this.directives = directives;

    public virtual void VisitAlias(AliasDeclaration node)
    {
        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this);

        node.Type.Accept(this);
    }

    public virtual void VisitArrayAccess(ArrayAccessExpression node)
    {
        node.Member.Accept(this);
        node.Index.Accept(this);
    }

    public virtual void VisitArrayType(ArrayType node)
    {
        node.ElementType.Accept(this);
    }

    public virtual void VisitBinaryExpression(BinaryExpression node)
    {
        node.Left.Accept(this);
        node.Right.Accept(this);
    }

    public virtual void VisitBlock(BlockStatement node)
    {
        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);
    }

    public virtual void VisitBreak(Break node)
    {
    }

    public virtual void VisitCall(CallExpression node)
    {
        node.Member.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);
    }

    public virtual void VisitCast(CastExpression node)
    {
        node.Type.Accept(this);
        node.Expression.Accept(this);
    }

    public virtual void VisitConstructor(ConstructorDeclaration node)
    {
        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.Body.Accept(this);
    }

    public virtual void VisitContinue(Continue node)
    {
    }

    public virtual void VisitDiscriminatedUnion(DiscriminatedUnion node)
    {
        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);
    }

    public virtual void VisitExpressionBlock(ExpressionBlock node)
    {
        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);
    }

    public virtual void VisitExpressionStatement(ExpressionStatement node)
    {
        node.Expression.Accept(this);
    }

    public virtual void VisitFakeDeclaration(FakeDeclaration node)
    {
    }

    public virtual void VisitFakeExpression(FakeExpression node)
    {
    }

    public virtual void VisitFakeStatement(FakeStatement node)
    {
    }

    public virtual void VisitFakeType(FakeType node)
    {
    }

    public virtual void VisitFunction(FunctionDeclaration node)
    {
        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);
    }

    public virtual void VisitFunctionType(FunctionType node)
    {
        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);
    }

    public virtual void VisitGenericType(GenericApplication node)
    {
        for (var i = 0; i < node.TypeArguments.Count; i++)
            node.TypeArguments[i].Accept(this);
    }

    public virtual void VisitGoTo(GoTo node)
    {
    }

    public virtual void VisitIfDirective(IfDirective node)
    {
        if (directives.Contains(node.DirectiveName))
        {
            for (var i = 0; i < node.Then.Count; i++)
                node.Then[i].Accept(this);
        }
        else
        {
            for (var i = 0; i < node.Else.Count; i++)
                node.Else[i].Accept(this);
        }
    }

    public virtual void VisitIf(IfStatement node)
    {
        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);
    }

    public virtual void VisitInterface(Interface node)
    {
        for (var i = 0; i < node.Properties.Count; i++)
            node.Properties[i].Accept(this);

        for (var i = 0; i < node.Methods.Count; i++)
            node.Methods[i].Accept(this);
    }

    public virtual void VisitInterfaceProperty(InterfaceProperty node)
    {
        node.Type.Accept(this);
    }

    public virtual void VisitInterfaceMethod(InterfaceMethod node)
    {
        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);
    }

    public virtual void VisitIsExpression(IsExpression node)
    {
        node.Expression.Accept(this);
        node.Type.Accept(this);
    }

    public virtual void VisitLabel(Label node)
    {
    }

    public virtual void VisitLiteral(LiteralExpression node)
    {
    }

    public virtual void VisitMemberAccess(MemberAccessExpression node)
    {
        node.Member?.Accept(this);
    }

    public virtual void VisitMethod(MethodDeclaration node)
    {
        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);
    }

    public virtual void VisitNamespace(Namespace node)
    {
    }

    public virtual void VisitNewArray(NewArrayExpression node)
    {
        node.Type.Accept(this);
        node.Size.Accept(this);
    }

    public virtual void VisitNewObject(NewObjectExpression node)
    {
        node.Type.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);
    }

    public virtual void VisitNull(NullExpression node)
    {
    }

    public virtual void VisitParameter(Parameter node)
    {
        node.Type.Accept(this);
    }

    public virtual void VisitProperty(PropertyDeclaration node)
    {
        node.Type.Accept(this);
        node.Getter?.Accept(this);
        node.Setter?.Accept(this);
    }

    public virtual void VisitGetter(PropertyGetter node)
    {
        node.Body?.Accept(this);
    }

    public virtual void VisitSetter(PropertySetter node)
    {
        node.Body?.Accept(this);
    }

    public virtual void VisitReturn(ReturnStatement node)
    {
        node.Expression?.Accept(this);
    }

    public virtual void VisitTree(SemanticTree node)
    {
        node.Namespace.Accept(this);

        for (var i = 0; i < node.UseNodes.Count; i++)
            node.UseNodes[i].Accept(this);

        for (var i = 0; i < node.Declarations.Count; i++)
            node.Declarations[i].Accept(this);
    }

    public virtual void VisitTuple(TupleExpression node)
    {
        for (var i = 0; i < node.Expressions.Count; i++)
            node.Expressions[i].Accept(this);
    }

    public virtual void VisitTupleType(TupleType node)
    {
        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);
    }

    public virtual void VisitType(TypeDeclaration node)
    {
        for (var i = 0; i < node.GenericArguments.Count; i++)
            node.GenericArguments[i].Accept(this);

        for (var i = 0; i < node.Interfaces.Count; i++)
            node.Interfaces[i].Accept(this);

        for (var i = 0; i < node.Properties.Count; i++)
            node.Properties[i].Accept(this);

        for (var i = 0; i < node.Constructors.Count; i++)
            node.Constructors[i].Accept(this);

        for (var i = 0; i < node.Methods.Count; i++)
            node.Methods[i].Accept(this);
    }

    public virtual void VisitTypeRef(TypeRef node)
    {
    }

    public virtual void VisitUnaryExpression(UnaryExpression node)
    {
        node.Operand.Accept(this);
    }

    public virtual void VisitUse(Use node)
    {
    }

    public virtual void VisitVariable(VariableDeclaration node)
    {
        node.Type.Accept(this);
        node.Expression.Accept(this);
    }

    public virtual void VisitWhile(While node)
    {
        node.Condition.Accept(this);
        node.Body.Accept(this);
    }
}