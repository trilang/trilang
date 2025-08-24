using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor : IVisitor
{
    public virtual void VisitArrayAccess(ArrayAccessExpressionNode node)
    {
        VisitArrayAccessEnter(node);

        node.Member.Accept(this);
        node.Index.Accept(this);

        VisitArrayAccessExit(node);
    }

    protected virtual void VisitArrayAccessEnter(ArrayAccessExpressionNode node)
    {
    }

    protected virtual void VisitArrayAccessExit(ArrayAccessExpressionNode node)
    {
    }

    public virtual void VisitArrayType(ArrayTypeNode node)
    {
        VisitArrayTypeEnter(node);

        node.ElementType.Accept(this);

        VisitArrayTypeExit(node);
    }

    protected virtual void VisitArrayTypeEnter(ArrayTypeNode node)
    {
    }

    protected virtual void VisitArrayTypeExit(ArrayTypeNode node)
    {
    }

    public virtual void VisitBinaryExpression(BinaryExpressionNode node)
    {
        VisitBinaryExpressionEnter(node);

        node.Left.Accept(this);
        node.Right.Accept(this);

        VisitBinaryExpressionExit(node);
    }

    protected virtual void VisitBinaryExpressionEnter(BinaryExpressionNode node)
    {
    }

    protected virtual void VisitBinaryExpressionExit(BinaryExpressionNode node)
    {
    }

    public virtual void VisitBlock(BlockStatementNode node)
    {
        VisitBlockEnter(node);

        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);

        VisitBlockExit(node);
    }

    protected virtual void VisitBlockEnter(BlockStatementNode node)
    {
    }

    protected virtual void VisitBlockExit(BlockStatementNode node)
    {
    }

    public virtual void VisitBreak(BreakNode node)
    {
        VisitBreakEnter(node);
        VisitBreakExit(node);
    }

    protected virtual void VisitBreakEnter(BreakNode node)
    {
    }

    protected virtual void VisitBreakExit(BreakNode node)
    {
    }

    public virtual void VisitCall(CallExpressionNode node)
    {
        VisitCallEnter(node);

        node.Member.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        VisitCallExit(node);
    }

    protected virtual void VisitCallEnter(CallExpressionNode node)
    {
    }

    protected virtual void VisitCallExit(CallExpressionNode node)
    {
    }

    public void VisitCast(CastExpressionNode node)
    {
        VisitCastEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitCastExit(node);
    }

    protected virtual void VisitCastEnter(CastExpressionNode node)
    {
    }

    protected virtual void VisitCastExit(CastExpressionNode node)
    {
    }

    public virtual void VisitConstructor(ConstructorDeclarationNode node)
    {
        VisitConstructorEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.Body.Accept(this);

        VisitConstructorExit(node);
    }

    protected virtual void VisitConstructorEnter(ConstructorDeclarationNode node)
    {
    }

    protected virtual void VisitConstructorExit(ConstructorDeclarationNode node)
    {
    }

    public virtual void VisitContinue(ContinueNode node)
    {
        VisitContinueEnter(node);
        VisitContinueExit(node);
    }

    protected virtual void VisitContinueEnter(ContinueNode node)
    {
    }

    protected virtual void VisitContinueExit(ContinueNode node)
    {
    }

    public virtual void VisitDiscriminatedUnion(DiscriminatedUnionNode node)
    {
        VisitDiscriminatedUnionEnter(node);

        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);

        VisitDiscriminatedUnionExit(node);
    }

    protected virtual void VisitDiscriminatedUnionEnter(DiscriminatedUnionNode node)
    {
    }

    protected virtual void VisitDiscriminatedUnionExit(DiscriminatedUnionNode node)
    {
    }

    public void VisitExpressionBlock(ExpressionBlockNode node)
    {
        VisitExpressionBlockEnter(node);

        for (var i = 0; i < node.Statements.Count; i++)
            node.Statements[i].Accept(this);

        VisitExpressionBlockExit(node);
    }

    protected virtual void VisitExpressionBlockEnter(ExpressionBlockNode node)
    {
    }

    protected virtual void VisitExpressionBlockExit(ExpressionBlockNode node)
    {
    }

    public virtual void VisitExpressionStatement(ExpressionStatementNode node)
    {
        VisitExpressionStatementEnter(node);

        node.Expression.Accept(this);

        VisitExpressionStatementExit(node);
    }

    protected virtual void VisitExpressionStatementEnter(ExpressionStatementNode node)
    {
    }

    protected virtual void VisitExpressionStatementExit(ExpressionStatementNode node)
    {
    }

    public virtual void VisitFunction(FunctionDeclarationNode node)
    {
        VisitFunctionEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body?.Accept(this);

        VisitFunctionExit(node);
    }

    protected virtual void VisitFunctionEnter(FunctionDeclarationNode node)
    {
    }

    protected virtual void VisitFunctionExit(FunctionDeclarationNode node)
    {
    }

    public virtual void VisitFunctionType(FunctionTypeNode node)
    {
        VisitFunctionTypeEnter(node);

        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);

        VisitFunctionTypeExit(node);
    }

    protected virtual void VisitFunctionTypeEnter(FunctionTypeNode node)
    {
    }

    protected virtual void VisitFunctionTypeExit(FunctionTypeNode node)
    {
    }

    public virtual void VisitGenericType(GenericTypeNode node)
    {
        VisitGenericTypeEnter(node);

        for (var i = 0; i < node.TypeArguments.Count; i++)
            node.TypeArguments[i].Accept(this);

        VisitGenericTypeExit(node);
    }

    protected virtual void VisitGenericTypeEnter(GenericTypeNode node)
    {
    }

    protected virtual void VisitGenericTypeExit(GenericTypeNode node)
    {
    }

    public virtual void VisitGoTo(GoToNode node)
    {
        VisitGoToEnter(node);
        VisitGoToExit(node);
    }

    protected virtual void VisitGoToEnter(GoToNode node)
    {
    }

    protected virtual void VisitGoToExit(GoToNode node)
    {
    }

    public virtual void VisitIfDirective(IfDirectiveNode node)
    {
        VisitIfDirectiveEnter(node);

        for (var i = 0; i < node.Then.Count; i++)
            node.Then[i].Accept(this);

        for (var i = 0; i < node.Else.Count; i++)
            node.Else[i].Accept(this);

        VisitIfDirectiveExit(node);
    }

    protected virtual void VisitIfDirectiveEnter(IfDirectiveNode node)
    {
    }

    protected virtual void VisitIfDirectiveExit(IfDirectiveNode node)
    {
    }

    public virtual void VisitIf(IfStatementNode node)
    {
        VisitIfEnter(node);

        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        VisitIfExit(node);
    }

    protected virtual void VisitIfEnter(IfStatementNode node)
    {
    }

    protected virtual void VisitIfExit(IfStatementNode node)
    {
    }

    public virtual void VisitInterface(InterfaceNode node)
    {
        VisitInterfaceEnter(node);

        for (var i = 0; i < node.Properties.Count; i++)
            node.Properties[i].Accept(this);

        for (var i = 0; i < node.Methods.Count; i++)
            node.Methods[i].Accept(this);

        VisitInterfaceExit(node);
    }

    protected virtual void VisitInterfaceEnter(InterfaceNode node)
    {
    }

    protected virtual void VisitInterfaceExit(InterfaceNode node)
    {
    }

    public virtual void VisitInterfaceProperty(InterfacePropertyNode node)
    {
        VisitInterfacePropertyEnter(node);

        node.Type.Accept(this);

        VisitInterfacePropertyExit(node);
    }

    protected virtual void VisitInterfacePropertyEnter(InterfacePropertyNode node)
    {
    }

    protected virtual void VisitInterfacePropertyExit(InterfacePropertyNode node)
    {
    }

    public virtual void VisitInterfaceMethod(InterfaceMethodNode node)
    {
        VisitInterfaceMethodEnter(node);

        for (var i = 0; i < node.ParameterTypes.Count; i++)
            node.ParameterTypes[i].Accept(this);

        node.ReturnType.Accept(this);

        VisitInterfaceMethodExit(node);
    }

    protected virtual void VisitInterfaceMethodEnter(InterfaceMethodNode node)
    {
    }

    protected virtual void VisitInterfaceMethodExit(InterfaceMethodNode node)
    {
    }

    public virtual void VisitAsExpression(IsExpressionNode node)
    {
        VisitAsExpressionEnter(node);

        node.Expression.Accept(this);
        node.Type.Accept(this);

        VisitAsExpressionExit(node);
    }

    protected virtual void VisitAsExpressionEnter(IsExpressionNode node)
    {
    }

    protected virtual void VisitAsExpressionExit(IsExpressionNode node)
    {
    }

    public virtual void VisitLabel(LabelNode node)
    {
        VisitLabelEnter(node);
        VisitLabelExit(node);
    }

    protected virtual void VisitLabelEnter(LabelNode node)
    {
    }

    protected virtual void VisitLabelExit(LabelNode node)
    {
    }

    public virtual void VisitLiteral(LiteralExpressionNode node)
    {
        VisitLiteralEnter(node);
        VisitLiteralExit(node);
    }

    protected virtual void VisitLiteralEnter(LiteralExpressionNode node)
    {
    }

    protected virtual void VisitLiteralExit(LiteralExpressionNode node)
    {
    }

    public virtual void VisitMemberAccess(MemberAccessExpressionNode node)
    {
        VisitMemberAccessEnter(node);

        node.Member?.Accept(this);

        VisitMemberAccessExit(node);
    }

    protected virtual void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
    }

    protected virtual void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
    }

    public virtual void VisitMethod(MethodDeclarationNode node)
    {
        VisitMethodEnter(node);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);

        VisitMethodExit(node);
    }

    protected virtual void VisitMethodEnter(MethodDeclarationNode node)
    {
    }

    protected virtual void VisitMethodExit(MethodDeclarationNode node)
    {
    }

    public virtual void VisitNewArray(NewArrayExpressionNode node)
    {
        VisitNewArrayEnter(node);

        node.Type.Accept(this);
        node.Size.Accept(this);

        VisitNewArrayExit(node);
    }

    protected virtual void VisitNewArrayEnter(NewArrayExpressionNode node)
    {
    }

    protected virtual void VisitNewArrayExit(NewArrayExpressionNode node)
    {
    }

    public virtual void VisitNewObject(NewObjectExpressionNode node)
    {
        VisitNewObjectEnter(node);

        node.Type.Accept(this);

        for (var i = 0; i < node.Parameters.Count; i++)
            node.Parameters[i].Accept(this);

        VisitNewObjectExit(node);
    }

    protected virtual void VisitNewObjectEnter(NewObjectExpressionNode node)
    {
    }

    protected virtual void VisitNewObjectExit(NewObjectExpressionNode node)
    {
    }

    public virtual void VisitNull(NullExpressionNode node)
    {
        VisitNullEnter(node);
        VisitNullExit(node);
    }

    protected virtual void VisitNullEnter(NullExpressionNode node)
    {
    }

    protected virtual void VisitNullExit(NullExpressionNode node)
    {
    }

    public virtual void VisitReturn(ReturnStatementNode node)
    {
        VisitReturnEnter(node);

        node.Expression?.Accept(this);

        VisitReturnExit(node);
    }

    protected virtual void VisitReturnEnter(ReturnStatementNode node)
    {
    }

    protected virtual void VisitReturnExit(ReturnStatementNode node)
    {
    }

    public virtual void VisitParameter(ParameterNode node)
    {
        VisitParameterEnter(node);

        node.Type.Accept(this);

        VisitParameterExit(node);
    }

    protected virtual void VisitParameterEnter(ParameterNode node)
    {
    }

    protected virtual void VisitParameterExit(ParameterNode node)
    {
    }

    public virtual void VisitProperty(PropertyDeclarationNode node)
    {
        VisitPropertyEnter(node);

        node.Type.Accept(this);
        node.Getter?.Accept(this);
        node.Setter?.Accept(this);

        VisitPropertyExit(node);
    }

    protected virtual void VisitPropertyEnter(PropertyDeclarationNode node)
    {
    }

    protected virtual void VisitPropertyExit(PropertyDeclarationNode node)
    {
    }

    public virtual void VisitGetter(PropertyGetterNode node)
    {
        VisitGetterEnter(node);

        node.Body?.Accept(this);

        VisitGetterExit(node);
    }

    protected virtual void VisitGetterEnter(PropertyGetterNode node)
    {
    }

    protected virtual void VisitGetterExit(PropertyGetterNode node)
    {
    }

    public virtual void VisitSetter(PropertySetterNode node)
    {
        VisitSetterEnter(node);

        node.Body?.Accept(this);

        VisitSetterExit(node);
    }

    protected virtual void VisitSetterEnter(PropertySetterNode node)
    {
    }

    protected virtual void VisitSetterExit(PropertySetterNode node)
    {
    }

    public virtual void VisitTree(SyntaxTree node)
    {
        VisitTreeEnter(node);

        for (var i = 0; i < node.Declarations.Count; i++)
            node.Declarations[i].Accept(this);

        VisitTreeExit(node);
    }

    protected virtual void VisitTreeEnter(SyntaxTree node)
    {
    }

    protected virtual void VisitTreeExit(SyntaxTree node)
    {
    }

    public virtual void VisitTuple(TupleExpressionNode node)
    {
        VisitTupleEnter(node);

        for (var i = 0; i < node.Expressions.Count; i++)
            node.Expressions[i].Accept(this);

        VisitTupleExit(node);
    }

    protected virtual void VisitTupleEnter(TupleExpressionNode node)
    {
    }

    protected virtual void VisitTupleExit(TupleExpressionNode node)
    {
    }

    public virtual void VisitTupleType(TupleTypeNode node)
    {
        VisitTupleTypeEnter(node);

        for (var i = 0; i < node.Types.Count; i++)
            node.Types[i].Accept(this);

        VisitTupleTypeExit(node);
    }

    protected virtual void VisitTupleTypeEnter(TupleTypeNode node)
    {
    }

    protected virtual void VisitTupleTypeExit(TupleTypeNode node)
    {
    }

    public virtual void VisitTypeAlias(TypeAliasDeclarationNode node)
    {
        VisitTypeAliasEnter(node);

        node.Type.Accept(this);

        VisitTypeAliasExit(node);
    }

    protected virtual void VisitTypeAliasEnter(TypeAliasDeclarationNode node)
    {
    }

    protected virtual void VisitTypeAliasExit(TypeAliasDeclarationNode node)
    {
    }

    public virtual void VisitType(TypeDeclarationNode node)
    {
        VisitTypeEnter(node);

        for (var i = 0; i < node.Properties.Count; i++)
            node.Properties[i].Accept(this);

        for (var i = 0; i < node.Methods.Count; i++)
            node.Methods[i].Accept(this);

        for (var i = 0; i < node.Constructors.Count; i++)
            node.Constructors[i].Accept(this);

        VisitTypeExit(node);
    }

    protected virtual void VisitTypeEnter(TypeDeclarationNode node)
    {
    }

    protected virtual void VisitTypeExit(TypeDeclarationNode node)
    {
    }

    public virtual void VisitTypeNode(TypeNode node)
    {
        VisitTypeNodeEnter(node);
        VisitTypeNodeExit(node);
    }

    protected virtual void VisitTypeNodeEnter(TypeNode node)
    {
    }

    protected virtual void VisitTypeNodeExit(TypeNode node)
    {
    }

    public virtual void VisitUnaryExpression(UnaryExpressionNode node)
    {
        VisitUnaryExpressionEnter(node);

        node.Operand.Accept(this);

        VisitUnaryExpressionExit(node);
    }

    protected virtual void VisitUnaryExpressionEnter(UnaryExpressionNode node)
    {
    }

    protected virtual void VisitUnaryExpressionExit(UnaryExpressionNode node)
    {
    }

    public virtual void VisitVariable(VariableDeclarationStatementNode node)
    {
        VisitVariableEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitVariableExit(node);
    }

    protected virtual void VisitVariableEnter(VariableDeclarationStatementNode node)
    {
    }

    protected virtual void VisitVariableExit(VariableDeclarationStatementNode node)
    {
    }

    public virtual void VisitWhile(WhileNode node)
    {
        VisitWhileEnter(node);

        node.Condition.Accept(this);
        node.Body.Accept(this);

        VisitWhileExit(node);
    }

    protected virtual void VisitWhileEnter(WhileNode node)
    {
    }

    protected virtual void VisitWhileExit(WhileNode node)
    {
    }
}