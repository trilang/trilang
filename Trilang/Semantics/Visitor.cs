using Trilang.Semantics.Model;

namespace Trilang.Semantics;

internal abstract class Visitor : IVisitor
{
    public virtual void VisitAlias(AliasDeclaration node)
    {
        VisitAliasEnter(node);

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this);

        node.Type.Accept(this);

        VisitAliasExit(node);
    }

    protected virtual void VisitAliasEnter(AliasDeclaration node)
    {
    }

    protected virtual void VisitAliasExit(AliasDeclaration node)
    {
    }

    public virtual void VisitArrayAccess(ArrayAccessExpression node)
    {
        VisitArrayAccessEnter(node);

        node.Member.Accept(this);
        node.Index.Accept(this);

        VisitArrayAccessExit(node);
    }

    protected virtual void VisitArrayAccessEnter(ArrayAccessExpression node)
    {
    }

    protected virtual void VisitArrayAccessExit(ArrayAccessExpression node)
    {
    }

    public virtual void VisitArrayType(ArrayType node)
    {
        VisitArrayTypeEnter(node);

        node.ElementType.Accept(this);

        VisitArrayTypeExit(node);
    }

    protected virtual void VisitArrayTypeEnter(ArrayType node)
    {
    }

    protected virtual void VisitArrayTypeExit(ArrayType node)
    {
    }

    public virtual void VisitBinaryExpression(BinaryExpression node)
    {
        VisitBinaryExpressionEnter(node);

        node.Left.Accept(this);
        node.Right.Accept(this);

        VisitBinaryExpressionExit(node);
    }

    protected virtual void VisitBinaryExpressionEnter(BinaryExpression node)
    {
    }

    protected virtual void VisitBinaryExpressionExit(BinaryExpression node)
    {
    }

    public virtual void VisitBlock(BlockStatement node)
    {
        VisitBlockEnter(node);

        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);

        VisitBlockExit(node);
    }

    protected virtual void VisitBlockEnter(BlockStatement node)
    {
    }

    protected virtual void VisitBlockExit(BlockStatement node)
    {
    }

    public virtual void VisitBreak(Break node)
    {
        VisitBreakEnter(node);
        VisitBreakExit(node);
    }

    protected virtual void VisitBreakEnter(Break node)
    {
    }

    protected virtual void VisitBreakExit(Break node)
    {
    }

    public virtual void VisitCall(CallExpression node)
    {
        VisitCallEnter(node);

        node.Member.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        VisitCallExit(node);
    }

    protected virtual void VisitCallEnter(CallExpression node)
    {
    }

    protected virtual void VisitCallExit(CallExpression node)
    {
    }

    public void VisitCast(CastExpression node)
    {
        VisitCastEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitCastExit(node);
    }

    protected virtual void VisitCastEnter(CastExpression node)
    {
    }

    protected virtual void VisitCastExit(CastExpression node)
    {
    }

    public virtual void VisitConstructor(ConstructorDeclaration node)
    {
        VisitConstructorEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.Body.Accept(this);

        VisitConstructorExit(node);
    }

    protected virtual void VisitConstructorEnter(ConstructorDeclaration node)
    {
    }

    protected virtual void VisitConstructorExit(ConstructorDeclaration node)
    {
    }

    public virtual void VisitContinue(Continue node)
    {
        VisitContinueEnter(node);
        VisitContinueExit(node);
    }

    protected virtual void VisitContinueEnter(Continue node)
    {
    }

    protected virtual void VisitContinueExit(Continue node)
    {
    }

    public virtual void VisitDiscriminatedUnion(DiscriminatedUnion node)
    {
        VisitDiscriminatedUnionEnter(node);

        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);

        VisitDiscriminatedUnionExit(node);
    }

    protected virtual void VisitDiscriminatedUnionEnter(DiscriminatedUnion node)
    {
    }

    protected virtual void VisitDiscriminatedUnionExit(DiscriminatedUnion node)
    {
    }

    public virtual void VisitExpressionBlock(ExpressionBlock node)
    {
        VisitExpressionBlockEnter(node);

        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);

        VisitExpressionBlockExit(node);
    }

    protected virtual void VisitExpressionBlockEnter(ExpressionBlock node)
    {
    }

    protected virtual void VisitExpressionBlockExit(ExpressionBlock node)
    {
    }

    public virtual void VisitExpressionStatement(ExpressionStatement node)
    {
        VisitExpressionStatementEnter(node);

        node.Expression.Accept(this);

        VisitExpressionStatementExit(node);
    }

    protected virtual void VisitExpressionStatementEnter(ExpressionStatement node)
    {
    }

    protected virtual void VisitExpressionStatementExit(ExpressionStatement node)
    {
    }

    public virtual void VisitFakeDeclaration(FakeDeclaration node)
    {
        VisitFakeDeclarationEnter(node);
        VisitFakeDeclarationExit(node);
    }

    protected virtual void VisitFakeDeclarationEnter(FakeDeclaration node)
    {
    }

    protected virtual void VisitFakeDeclarationExit(FakeDeclaration node)
    {
    }

    public virtual void VisitFakeExpression(FakeExpression node)
    {
        VisitFakeExpressionEnter(node);
        VisitFakeExpressionExit(node);
    }

    protected virtual void VisitFakeExpressionEnter(FakeExpression node)
    {
    }

    protected virtual void VisitFakeExpressionExit(FakeExpression node)
    {
    }

    public virtual void VisitFakeStatement(FakeStatement node)
    {
        VisitFakeStatementEnter(node);
        VisitFakeStatementExit(node);
    }

    protected virtual void VisitFakeStatementEnter(FakeStatement node)
    {
    }

    protected virtual void VisitFakeStatementExit(FakeStatement node)
    {
    }

    public virtual void VisitFakeType(FakeType node)
    {
        VisitFakeTypeEnter(node);
        VisitFakeTypeExit(node);
    }

    protected virtual void VisitFakeTypeEnter(FakeType node)
    {
    }

    protected virtual void VisitFakeTypeExit(FakeType node)
    {
    }

    public virtual void VisitFunction(FunctionDeclaration node)
    {
        VisitFunctionEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);

        VisitFunctionExit(node);
    }

    protected virtual void VisitFunctionEnter(FunctionDeclaration node)
    {
    }

    protected virtual void VisitFunctionExit(FunctionDeclaration node)
    {
    }

    public virtual void VisitFunctionType(FunctionType node)
    {
        VisitFunctionTypeEnter(node);

        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);

        VisitFunctionTypeExit(node);
    }

    protected virtual void VisitFunctionTypeEnter(FunctionType node)
    {
    }

    protected virtual void VisitFunctionTypeExit(FunctionType node)
    {
    }

    public virtual void VisitGenericTypeRef(GenericTypeRef node)
    {
        VisitGenericTypeRefEnter(node);

        for (var i = 0; i < node.TypeArguments.Count; i++)
            node.TypeArguments[i].Accept(this);

        VisitGenericTypeRefExit(node);
    }

    protected virtual void VisitGenericTypeRefEnter(GenericTypeRef node)
    {
    }

    protected virtual void VisitGenericTypeRefExit(GenericTypeRef node)
    {
    }

    public virtual void VisitGoTo(GoTo node)
    {
        VisitGoToEnter(node);
        VisitGoToExit(node);
    }

    protected virtual void VisitGoToEnter(GoTo node)
    {
    }

    protected virtual void VisitGoToExit(GoTo node)
    {
    }

    public virtual void VisitIfDirective(IfDirective node)
    {
        VisitIfDirectiveEnter(node);

        for (var i = 0; i < node.Then.Count; i++)
            node.Then[i].Accept(this);

        for (var i = 0; i < node.Else.Count; i++)
            node.Else[i].Accept(this);

        VisitIfDirectiveExit(node);
    }

    protected virtual void VisitIfDirectiveEnter(IfDirective node)
    {
    }

    protected virtual void VisitIfDirectiveExit(IfDirective node)
    {
    }

    public virtual void VisitIf(IfStatement node)
    {
        VisitIfEnter(node);

        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        VisitIfExit(node);
    }

    protected virtual void VisitIfEnter(IfStatement node)
    {
    }

    protected virtual void VisitIfExit(IfStatement node)
    {
    }

    public virtual void VisitInterface(Interface node)
    {
        VisitInterfaceEnter(node);

        for (var i = 0; i < node.Properties.Count; i++)
            node.Properties[i].Accept(this);

        for (var i = 0; i < node.Methods.Count; i++)
            node.Methods[i].Accept(this);

        VisitInterfaceExit(node);
    }

    protected virtual void VisitInterfaceEnter(Interface node)
    {
    }

    protected virtual void VisitInterfaceExit(Interface node)
    {
    }

    public virtual void VisitInterfaceProperty(InterfaceProperty node)
    {
        VisitInterfacePropertyEnter(node);

        node.Type.Accept(this);

        VisitInterfacePropertyExit(node);
    }

    protected virtual void VisitInterfacePropertyEnter(InterfaceProperty node)
    {
    }

    protected virtual void VisitInterfacePropertyExit(InterfaceProperty node)
    {
    }

    public virtual void VisitInterfaceMethod(InterfaceMethod node)
    {
        VisitInterfaceMethodEnter(node);

        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);

        VisitInterfaceMethodExit(node);
    }

    protected virtual void VisitInterfaceMethodEnter(InterfaceMethod node)
    {
    }

    protected virtual void VisitInterfaceMethodExit(InterfaceMethod node)
    {
    }

    public virtual void VisitIsExpression(IsExpression node)
    {
        VisitIsExpressionEnter(node);

        node.Expression.Accept(this);
        node.Type.Accept(this);

        VisitIsExpressionExit(node);
    }

    protected virtual void VisitIsExpressionEnter(IsExpression node)
    {
    }

    protected virtual void VisitIsExpressionExit(IsExpression node)
    {
    }

    public virtual void VisitLabel(Label node)
    {
        VisitLabelEnter(node);
        VisitLabelExit(node);
    }

    protected virtual void VisitLabelEnter(Label node)
    {
    }

    protected virtual void VisitLabelExit(Label node)
    {
    }

    public virtual void VisitLiteral(LiteralExpression node)
    {
        VisitLiteralEnter(node);
        VisitLiteralExit(node);
    }

    protected virtual void VisitLiteralEnter(LiteralExpression node)
    {
    }

    protected virtual void VisitLiteralExit(LiteralExpression node)
    {
    }

    public virtual void VisitMemberAccess(MemberAccessExpression node)
    {
        VisitMemberAccessEnter(node);

        node.Member?.Accept(this);

        VisitMemberAccessExit(node);
    }

    protected virtual void VisitMemberAccessEnter(MemberAccessExpression node)
    {
    }

    protected virtual void VisitMemberAccessExit(MemberAccessExpression node)
    {
    }

    public virtual void VisitMethod(MethodDeclaration node)
    {
        VisitMethodEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);

        VisitMethodExit(node);
    }

    protected virtual void VisitMethodEnter(MethodDeclaration node)
    {
    }

    protected virtual void VisitMethodExit(MethodDeclaration node)
    {
    }

    public virtual void VisitNamespace(Namespace node)
    {
        VisitNamespaceEnter(node);
        VisitNamespaceExit(node);
    }

    protected virtual void VisitNamespaceEnter(Namespace node)
    {
    }

    protected virtual void VisitNamespaceExit(Namespace node)
    {
    }

    public virtual void VisitNewArray(NewArrayExpression node)
    {
        VisitNewArrayEnter(node);

        node.Type.Accept(this);
        node.Size.Accept(this);

        VisitNewArrayExit(node);
    }

    protected virtual void VisitNewArrayEnter(NewArrayExpression node)
    {
    }

    protected virtual void VisitNewArrayExit(NewArrayExpression node)
    {
    }

    public virtual void VisitNewObject(NewObjectExpression node)
    {
        VisitNewObjectEnter(node);

        node.Type.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        VisitNewObjectExit(node);
    }

    protected virtual void VisitNewObjectEnter(NewObjectExpression node)
    {
    }

    protected virtual void VisitNewObjectExit(NewObjectExpression node)
    {
    }

    public virtual void VisitNull(NullExpression node)
    {
        VisitNullEnter(node);
        VisitNullExit(node);
    }

    protected virtual void VisitNullEnter(NullExpression node)
    {
    }

    protected virtual void VisitNullExit(NullExpression node)
    {
    }

    public virtual void VisitReturn(ReturnStatement node)
    {
        VisitReturnEnter(node);

        node.Expression?.Accept(this);

        VisitReturnExit(node);
    }

    protected virtual void VisitReturnEnter(ReturnStatement node)
    {
    }

    protected virtual void VisitReturnExit(ReturnStatement node)
    {
    }

    public virtual void VisitParameter(Parameter node)
    {
        VisitParameterEnter(node);

        node.Type.Accept(this);

        VisitParameterExit(node);
    }

    protected virtual void VisitParameterEnter(Parameter node)
    {
    }

    protected virtual void VisitParameterExit(Parameter node)
    {
    }

    public virtual void VisitProperty(PropertyDeclaration node)
    {
        VisitPropertyEnter(node);

        node.Type.Accept(this);
        node.Getter?.Accept(this);
        node.Setter?.Accept(this);

        VisitPropertyExit(node);
    }

    protected virtual void VisitPropertyEnter(PropertyDeclaration node)
    {
    }

    protected virtual void VisitPropertyExit(PropertyDeclaration node)
    {
    }

    public virtual void VisitGetter(PropertyGetter node)
    {
        VisitGetterEnter(node);

        node.Body?.Accept(this);

        VisitGetterExit(node);
    }

    protected virtual void VisitGetterEnter(PropertyGetter node)
    {
    }

    protected virtual void VisitGetterExit(PropertyGetter node)
    {
    }

    public virtual void VisitSetter(PropertySetter node)
    {
        VisitSetterEnter(node);

        node.Body?.Accept(this);

        VisitSetterExit(node);
    }

    protected virtual void VisitSetterEnter(PropertySetter node)
    {
    }

    protected virtual void VisitSetterExit(PropertySetter node)
    {
    }

    public virtual void VisitTree(SemanticTree node)
    {
        VisitTreeEnter(node);

        for (var i = 0; i < node.Declarations.Count; i++)
            node.Declarations[i].Accept(this);

        VisitTreeExit(node);
    }

    protected virtual void VisitTreeEnter(SemanticTree node)
    {
    }

    protected virtual void VisitTreeExit(SemanticTree node)
    {
    }

    public virtual void VisitTuple(TupleExpression node)
    {
        VisitTupleEnter(node);

        for (var i = 0; i < node.Expressions.Count; i++)
            node.Expressions[i].Accept(this);

        VisitTupleExit(node);
    }

    protected virtual void VisitTupleEnter(TupleExpression node)
    {
    }

    protected virtual void VisitTupleExit(TupleExpression node)
    {
    }

    public virtual void VisitTupleType(TupleType node)
    {
        VisitTupleTypeEnter(node);

        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);

        VisitTupleTypeExit(node);
    }

    protected virtual void VisitTupleTypeEnter(TupleType node)
    {
    }

    protected virtual void VisitTupleTypeExit(TupleType node)
    {
    }

    public virtual void VisitType(TypeDeclaration node)
    {
        VisitTypeEnter(node);

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

        VisitTypeExit(node);
    }

    protected virtual void VisitTypeEnter(TypeDeclaration node)
    {
    }

    protected virtual void VisitTypeExit(TypeDeclaration node)
    {
    }

    public virtual void VisitTypeRef(TypeRef node)
    {
        VisitTypeRefEnter(node);
        VisitTypeRefExit(node);
    }

    protected virtual void VisitTypeRefEnter(TypeRef node)
    {
    }

    protected virtual void VisitTypeRefExit(TypeRef node)
    {
    }

    public virtual void VisitUnaryExpression(UnaryExpression node)
    {
        VisitUnaryExpressionEnter(node);

        node.Operand.Accept(this);

        VisitUnaryExpressionExit(node);
    }

    protected virtual void VisitUnaryExpressionEnter(UnaryExpression node)
    {
    }

    protected virtual void VisitUnaryExpressionExit(UnaryExpression node)
    {
    }

    public virtual void VisitUse(Use node)
    {
        VisitUseEnter(node);
        VisitUseExit(node);
    }

    protected virtual void VisitUseEnter(Use node)
    {
    }

    protected virtual void VisitUseExit(Use node)
    {
    }

    public virtual void VisitVariable(VariableDeclaration node)
    {
        VisitVariableEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitVariableExit(node);
    }

    protected virtual void VisitVariableEnter(VariableDeclaration node)
    {
    }

    protected virtual void VisitVariableExit(VariableDeclaration node)
    {
    }

    public virtual void VisitWhile(While node)
    {
        VisitWhileEnter(node);

        node.Condition.Accept(this);
        node.Body.Accept(this);

        VisitWhileExit(node);
    }

    protected virtual void VisitWhileEnter(While node)
    {
    }

    protected virtual void VisitWhileExit(While node)
    {
    }
}