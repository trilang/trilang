using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor : IVisitor
{
    public void VisitArrayAccess(ArrayAccessExpressionNode node)
    {
        VisitArrayAccessEnter(node);

        node.Member.Accept(this);

        VisitArrayAccessExit(node);
    }

    protected virtual void VisitArrayAccessEnter(ArrayAccessExpressionNode node)
    {
    }

    protected virtual void VisitArrayAccessExit(ArrayAccessExpressionNode node)
    {
    }

    public void VisitArrayType(ArrayTypeNode node)
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

    public void VisitAsExpression(AsExpressionNode node)
    {
        VisitAsExpressionEnter(node);

        node.Expression.Accept(this);
        node.Type.Accept(this);

        VisitAsExpressionExit(node);
    }

    protected virtual void VisitAsExpressionEnter(AsExpressionNode node)
    {
    }

    protected virtual void VisitAsExpressionExit(AsExpressionNode node)
    {
    }

    public void VisitBinaryExpression(BinaryExpressionNode node)
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

    public void VisitBlock(BlockStatementNode node)
    {
        VisitBlockEnter(node);

        foreach (var statement in node.Statements)
            statement.Accept(this);

        VisitBlockExit(node);
    }

    protected virtual void VisitBlockEnter(BlockStatementNode node)
    {
    }

    protected virtual void VisitBlockExit(BlockStatementNode node)
    {
    }

    public void VisitBreak(BreakNode node)
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

    public void VisitCall(CallExpressionNode node)
    {
        VisitCallEnter(node);

        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        VisitCallExit(node);
    }

    protected virtual void VisitCallEnter(CallExpressionNode node)
    {
    }

    protected virtual void VisitCallExit(CallExpressionNode node)
    {
    }

    public void VisitConstructor(ConstructorDeclarationNode node)
    {
        VisitConstructorEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.Body.Accept(this);

        VisitConstructorExit(node);
    }

    protected virtual void VisitConstructorEnter(ConstructorDeclarationNode node)
    {
    }

    protected virtual void VisitConstructorExit(ConstructorDeclarationNode node)
    {
    }

    public void VisitContinue(ContinueNode node)
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

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node)
    {
        VisitDiscriminatedUnionEnter(node);

        foreach (var type in node.Types)
            type.Accept(this);

        VisitDiscriminatedUnionExit(node);
    }

    protected virtual void VisitDiscriminatedUnionEnter(DiscriminatedUnionNode node)
    {
    }

    protected virtual void VisitDiscriminatedUnionExit(DiscriminatedUnionNode node)
    {
    }

    public void VisitExpressionStatement(ExpressionStatementNode node)
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

    public void VisitFunction(FunctionDeclarationNode node)
    {
        VisitFunctionEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

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

    public void VisitFunctionType(FunctionTypeNode node)
    {
        VisitFunctionTypeEnter(node);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        VisitFunctionTypeExit(node);
    }

    protected virtual void VisitFunctionTypeEnter(FunctionTypeNode node)
    {
    }

    protected virtual void VisitFunctionTypeExit(FunctionTypeNode node)
    {
    }

    public void VisitGenericType(GenericTypeNode node)
    {
        VisitGenericTypeEnter(node);

        foreach (var type in node.TypeArguments)
            type.Accept(this);

        VisitGenericTypeExit(node);
    }

    protected virtual void VisitGenericTypeEnter(GenericTypeNode node)
    {
    }

    protected virtual void VisitGenericTypeExit(GenericTypeNode node)
    {
    }

    public void VisitIfDirective(IfDirectiveNode node)
    {
        VisitIfDirectiveEnter(node);

        foreach (var then in node.Then)
            then.Accept(this);

        foreach (var @else in node.Else)
            @else.Accept(this);

        VisitIfDirectiveExit(node);
    }

    protected virtual void VisitIfDirectiveEnter(IfDirectiveNode node)
    {
    }

    protected virtual void VisitIfDirectiveExit(IfDirectiveNode node)
    {
    }

    public void VisitIf(IfStatementNode node)
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

    public void VisitInterface(InterfaceNode node)
    {
        VisitInterfaceEnter(node);

        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);

        VisitInterfaceExit(node);
    }

    protected virtual void VisitInterfaceEnter(InterfaceNode node)
    {
    }

    protected virtual void VisitInterfaceExit(InterfaceNode node)
    {
    }

    public void VisitInterfaceProperty(InterfacePropertyNode node)
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

    public void VisitInterfaceMethod(InterfaceMethodNode node)
    {
        VisitInterfaceMethodEnter(node);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        VisitInterfaceMethodExit(node);
    }

    protected virtual void VisitInterfaceMethodEnter(InterfaceMethodNode node)
    {
    }

    protected virtual void VisitInterfaceMethodExit(InterfaceMethodNode node)
    {
    }

    public void VisitLiteral(LiteralExpressionNode node)
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

    public void VisitMemberAccess(MemberAccessExpressionNode node)
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

    public void VisitMethod(MethodDeclarationNode node)
    {
        VisitMethodEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

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

    public void VisitNewArray(NewArrayExpressionNode node)
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

    public void VisitNewObject(NewObjectExpressionNode node)
    {
        VisitNewObjectEnter(node);

        node.Type.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        VisitNewObjectExit(node);
    }

    protected virtual void VisitNewObjectEnter(NewObjectExpressionNode node)
    {
    }

    protected virtual void VisitNewObjectExit(NewObjectExpressionNode node)
    {
    }

    public void VisitNull(NullExpressionNode node)
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

    public void VisitReturn(ReturnStatementNode node)
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

    public void VisitParameter(ParameterNode node)
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

    public void VisitProperty(PropertyDeclarationNode node)
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

    public void VisitGetter(PropertyGetterNode node)
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

    public void VisitSetter(PropertySetterNode node)
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

    public void VisitTree(SyntaxTree node)
    {
        VisitTreeEnter(node);

        foreach (var function in node.Declarations)
            function.Accept(this);

        VisitTreeExit(node);
    }

    protected virtual void VisitTreeEnter(SyntaxTree node)
    {
    }

    protected virtual void VisitTreeExit(SyntaxTree node)
    {
    }

    public void VisitTuple(TupleExpressionNode node)
    {
        VisitTupleEnter(node);

        foreach (var element in node.Expressions)
            element.Accept(this);

        VisitTupleExit(node);
    }

    protected virtual void VisitTupleEnter(TupleExpressionNode node)
    {
    }

    protected virtual void VisitTupleExit(TupleExpressionNode node)
    {
    }

    public void VisitTupleType(TupleTypeNode node)
    {
        VisitTupleTypeEnter(node);

        foreach (var type in node.Types)
            type.Accept(this);

        VisitTupleTypeExit(node);
    }

    protected virtual void VisitTupleTypeEnter(TupleTypeNode node)
    {
    }

    protected virtual void VisitTupleTypeExit(TupleTypeNode node)
    {
    }

    public void VisitTypeAlias(TypeAliasDeclarationNode node)
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

    public void VisitType(TypeDeclarationNode node)
    {
        VisitTypeEnter(node);

        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this);

        VisitTypeExit(node);
    }

    protected virtual void VisitTypeEnter(TypeDeclarationNode node)
    {
    }

    protected virtual void VisitTypeExit(TypeDeclarationNode node)
    {
    }

    public void VisitTypeNode(TypeNode node)
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

    public void VisitUnaryExpression(UnaryExpressionNode node)
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

    public void VisitVariable(VariableDeclarationStatementNode node)
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

    public void VisitWhile(WhileNode node)
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