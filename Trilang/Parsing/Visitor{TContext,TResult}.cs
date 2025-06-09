using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor<TContext, TResult> : IVisitor<TContext>
    where TContext : VisitorContext<TResult>
{
    public void VisitArrayAccess(ArrayAccessExpressionNode node, TContext context)
    {
        VisitArrayAccessEnter(node, context);

        node.Member.Accept(this, context);

        VisitArrayAccessExit(node, context);
    }

    protected virtual void VisitArrayAccessEnter(ArrayAccessExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitArrayAccessExit(ArrayAccessExpressionNode node, TContext context)
    {
    }

    public void VisitArrayType(ArrayTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitArrayTypeEnter(node, context);

        node.ElementType.Accept(this, context);

        VisitArrayTypeExit(node, context);
    }

    protected virtual void VisitArrayTypeEnter(ArrayTypeNode node, TContext context)
    {
    }

    protected virtual void VisitArrayTypeExit(ArrayTypeNode node, TContext context)
    {
    }

    public void VisitAsExpression(AsExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitAsExpressionEnter(node, context);

        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);

        VisitAsExpressionExit(node, context);
    }

    protected virtual void VisitAsExpressionEnter(AsExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitAsExpressionExit(AsExpressionNode node, TContext context)
    {
    }

    public void VisitBinaryExpression(BinaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitBinaryExpressionEnter(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);

        VisitBinaryExpressionExit(node, context);
    }

    protected virtual void VisitBinaryExpressionEnter(BinaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitBinaryExpressionExit(BinaryExpressionNode node, TContext context)
    {
    }

    public void VisitBlock(BlockStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitBlockEnter(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);

        VisitBlockExit(node, context);
    }

    protected virtual void VisitBlockEnter(BlockStatementNode node, TContext context)
    {
    }

    protected virtual void VisitBlockExit(BlockStatementNode node, TContext context)
    {
    }

    public void VisitBreak(BreakNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitBreakEnter(node, context);
        VisitBreakExit(node, context);
    }

    protected virtual void VisitBreakEnter(BreakNode node, TContext context)
    {
    }

    protected virtual void VisitBreakExit(BreakNode node, TContext context)
    {
    }

    public void VisitCall(CallExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitCallEnter(node, context);

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        VisitCallExit(node, context);
    }

    protected virtual void VisitCallEnter(CallExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitCallExit(CallExpressionNode node, TContext context)
    {
    }

    public void VisitConstructor(ConstructorDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitConstructorEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.Body.Accept(this, context);

        VisitConstructorExit(node, context);
    }

    protected virtual void VisitConstructorEnter(ConstructorDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitConstructorExit(ConstructorDeclarationNode node, TContext context)
    {
    }

    public void VisitContinue(ContinueNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitContinueEnter(node, context);
        VisitContinueExit(node, context);
    }

    protected virtual void VisitContinueEnter(ContinueNode node, TContext context)
    {
    }

    protected virtual void VisitContinueExit(ContinueNode node, TContext context)
    {
    }

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitDiscriminatedUnionEnter(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        VisitDiscriminatedUnionExit(node, context);
    }

    protected virtual void VisitDiscriminatedUnionEnter(DiscriminatedUnionNode node, TContext context)
    {
    }

    protected virtual void VisitDiscriminatedUnionExit(DiscriminatedUnionNode node, TContext context)
    {
    }

    public void VisitExpressionStatement(ExpressionStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitExpressionStatementEnter(node, context);

        node.Expression.Accept(this, context);

        VisitExpressionStatementExit(node, context);
    }

    protected virtual void VisitExpressionStatementEnter(ExpressionStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExpressionStatementExit(ExpressionStatementNode node, TContext context)
    {
    }

    public void VisitFunction(FunctionDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitFunctionEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body?.Accept(this, context);

        VisitFunctionExit(node, context);
    }

    protected virtual void VisitFunctionEnter(FunctionDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitFunctionExit(FunctionDeclarationNode node, TContext context)
    {
    }

    public void VisitFunctionType(FunctionTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitFunctionTypeEnter(node, context);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        VisitFunctionTypeExit(node, context);
    }

    protected virtual void VisitFunctionTypeEnter(FunctionTypeNode node, TContext context)
    {
    }

    protected virtual void VisitFunctionTypeExit(FunctionTypeNode node, TContext context)
    {
    }

    public void VisitGenericType(GenericTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitGenericTypeEnter(node, context);

        foreach (var type in node.TypeArguments)
            type.Accept(this, context);

        VisitGenericTypeExit(node, context);
    }

    protected virtual void VisitGenericTypeEnter(GenericTypeNode node, TContext context)
    {
    }

    protected virtual void VisitGenericTypeExit(GenericTypeNode node, TContext context)
    {
    }

    public void VisitIfDirective(IfDirectiveNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitIfDirectiveEnter(node, context);

        foreach (var then in node.Then)
            then.Accept(this, context);

        foreach (var @else in node.Else)
            @else.Accept(this, context);

        VisitIfDirectiveExit(node, context);
    }

    protected virtual void VisitIfDirectiveEnter(IfDirectiveNode node, TContext context)
    {
    }

    protected virtual void VisitIfDirectiveExit(IfDirectiveNode node, TContext context)
    {
    }

    public void VisitIf(IfStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitIfEnter(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);

        VisitIfExit(node, context);
    }

    protected virtual void VisitIfEnter(IfStatementNode node, TContext context)
    {
    }

    protected virtual void VisitIfExit(IfStatementNode node, TContext context)
    {
    }

    public void VisitInterface(InterfaceNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitInterfaceEnter(node, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);

        VisitInterfaceExit(node, context);
    }

    protected virtual void VisitInterfaceEnter(InterfaceNode node, TContext context)
    {
    }

    protected virtual void VisitInterfaceExit(InterfaceNode node, TContext context)
    {
    }

    public void VisitInterfaceProperty(InterfacePropertyNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitInterfacePropertyEnter(node, context);

        node.Type.Accept(this, context);

        VisitInterfacePropertyExit(node, context);
    }

    protected virtual void VisitInterfacePropertyEnter(InterfacePropertyNode node, TContext context)
    {
    }

    protected virtual void VisitInterfacePropertyExit(InterfacePropertyNode node, TContext context)
    {
    }

    public void VisitInterfaceMethod(InterfaceMethodNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitInterfaceMethodEnter(node, context);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        VisitInterfaceMethodExit(node, context);
    }

    protected virtual void VisitInterfaceMethodEnter(InterfaceMethodNode node, TContext context)
    {
    }

    protected virtual void VisitInterfaceMethodExit(InterfaceMethodNode node, TContext context)
    {
    }

    public void VisitLiteral(LiteralExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitLiteralEnter(node, context);
        VisitLiteralExit(node, context);
    }

    protected virtual void VisitLiteralEnter(LiteralExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitLiteralExit(LiteralExpressionNode node, TContext context)
    {
    }

    public void VisitMemberAccess(MemberAccessExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitMemberAccessEnter(node, context);

        node.Member?.Accept(this, context);

        VisitMemberAccessExit(node, context);
    }

    protected virtual void VisitMemberAccessEnter(MemberAccessExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitMemberAccessExit(MemberAccessExpressionNode node, TContext context)
    {
    }

    public void VisitMethod(MethodDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitMethodEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body.Accept(this, context);

        VisitMethodExit(node, context);
    }

    protected virtual void VisitMethodEnter(MethodDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitMethodExit(MethodDeclarationNode node, TContext context)
    {
    }

    public void VisitNewArray(NewArrayExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitNewArrayEnter(node, context);

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);

        VisitNewArrayExit(node, context);
    }

    protected virtual void VisitNewArrayEnter(NewArrayExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitNewArrayExit(NewArrayExpressionNode node, TContext context)
    {
    }

    public void VisitNewObject(NewObjectExpressionNode node, TContext context)
    {
        VisitNewObjectEnter(node, context);

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        VisitNewObjectExit(node, context);
    }

    protected virtual void VisitNewObjectEnter(NewObjectExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitNewObjectExit(NewObjectExpressionNode node, TContext context)
    {
    }

    public void VisitNull(NullExpressionNode node, TContext context)
    {
        VisitNullEnter(node, context);
        VisitNullExit(node, context);
    }

    protected virtual void VisitNullEnter(NullExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitNullExit(NullExpressionNode node, TContext context)
    {
    }

    public void VisitReturn(ReturnStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitReturnEnter(node, context);

        node.Expression?.Accept(this, context);

        VisitReturnExit(node, context);
    }

    protected virtual void VisitReturnEnter(ReturnStatementNode node, TContext context)
    {
    }

    protected virtual void VisitReturnExit(ReturnStatementNode node, TContext context)
    {
    }

    public void VisitParameter(ParameterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitParameterEnter(node, context);

        node.Type.Accept(this, context);

        VisitParameterExit(node, context);
    }

    protected virtual void VisitParameterEnter(ParameterNode node, TContext context)
    {
    }

    protected virtual void VisitParameterExit(ParameterNode node, TContext context)
    {
    }

    public void VisitProperty(PropertyDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitPropertyEnter(node, context);

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);

        VisitPropertyExit(node, context);
    }

    protected virtual void VisitPropertyEnter(PropertyDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitPropertyExit(PropertyDeclarationNode node, TContext context)
    {
    }

    public void VisitGetter(PropertyGetterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitGetterEnter(node, context);

        node.Body?.Accept(this, context);

        VisitGetterExit(node, context);
    }

    protected virtual void VisitGetterEnter(PropertyGetterNode node, TContext context)
    {
    }

    protected virtual void VisitGetterExit(PropertyGetterNode node, TContext context)
    {
    }

    public void VisitSetter(PropertySetterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitSetterEnter(node, context);

        node.Body?.Accept(this, context);

        VisitSetterExit(node, context);
    }

    protected virtual void VisitSetterEnter(PropertySetterNode node, TContext context)
    {
    }

    protected virtual void VisitSetterExit(PropertySetterNode node, TContext context)
    {
    }

    public void VisitTree(SyntaxTree node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTreeEnter(node, context);

        foreach (var function in node.Declarations)
            function.Accept(this, context);

        VisitTreeExit(node, context);
    }

    protected virtual void VisitTreeEnter(SyntaxTree node, TContext context)
    {
    }

    protected virtual void VisitTreeExit(SyntaxTree node, TContext context)
    {
    }

    public void VisitTuple(TupleExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTupleEnter(node, context);

        foreach (var element in node.Expressions)
            element.Accept(this, context);

        VisitTupleExit(node, context);
    }

    protected virtual void VisitTupleEnter(TupleExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitTupleExit(TupleExpressionNode node, TContext context)
    {
    }

    public void VisitTupleType(TupleTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTupleTypeEnter(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        VisitTupleTypeExit(node, context);
    }

    protected virtual void VisitTupleTypeEnter(TupleTypeNode node, TContext context)
    {
    }

    protected virtual void VisitTupleTypeExit(TupleTypeNode node, TContext context)
    {
    }


    public void VisitTypeAlias(TypeAliasDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTypeAliasEnter(node, context);

        node.Type.Accept(this, context);

        VisitTypeAliasExit(node, context);
    }

    protected virtual void VisitTypeAliasEnter(TypeAliasDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitTypeAliasExit(TypeAliasDeclarationNode node, TContext context)
    {
    }

    public void VisitType(TypeDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTypeEnter(node, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);

        VisitTypeExit(node, context);
    }

    protected virtual void VisitTypeEnter(TypeDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitTypeExit(TypeDeclarationNode node, TContext context)
    {
    }

    public void VisitTypeNode(TypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitTypeNodeEnter(node, context);
        VisitTypeNodeExit(node, context);
    }

    protected virtual void VisitTypeNodeEnter(TypeNode node, TContext context)
    {
    }

    protected virtual void VisitTypeNodeExit(TypeNode node, TContext context)
    {
    }

    public void VisitUnaryExpression(UnaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitUnaryExpressionEnter(node, context);

        node.Operand.Accept(this, context);

        VisitUnaryExpressionExit(node, context);
    }

    protected virtual void VisitUnaryExpressionEnter(UnaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitUnaryExpressionExit(UnaryExpressionNode node, TContext context)
    {
    }

    public void VisitVariable(VariableDeclarationStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitVariableEnter(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        VisitVariableExit(node, context);
    }

    protected virtual void VisitVariableEnter(VariableDeclarationStatementNode node, TContext context)
    {
    }

    protected virtual void VisitVariableExit(VariableDeclarationStatementNode node, TContext context)
    {
    }

    public void VisitWhile(WhileNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitWhileEnter(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);

        VisitWhileExit(node, context);
    }

    protected virtual void VisitWhileEnter(WhileNode node, TContext context)
    {
    }

    protected virtual void VisitWhileExit(WhileNode node, TContext context)
    {
    }
}