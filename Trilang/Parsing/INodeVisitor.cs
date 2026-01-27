using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface INodeVisitor
{
    void VisitTypeAlias(AliasDeclarationNode node);

    void VisitArrayAccess(ArrayAccessExpressionNode node);

    void VisitArrayType(ArrayTypeNode node);

    void VisitBinaryExpression(BinaryExpressionNode node);

    void VisitBlock(BlockStatementNode node);

    void VisitBreak(BreakNode node);

    void VisitCall(CallExpressionNode node);

    void VisitCast(CastExpressionNode node);

    void VisitConstructor(ConstructorDeclarationNode node);

    void VisitContinue(ContinueNode node);

    void VisitDiscriminatedUnion(DiscriminatedUnionNode node);

    void VisitExpressionStatement(ExpressionStatementNode node);

    void VisitFakeDeclaration(FakeDeclarationNode node);

    void VisitFakeExpression(FakeExpressionNode node);

    void VisitFakeStatement(FakeStatementNode node);

    void VisitFakeType(FakeTypeNode node);

    void VisitFunction(FunctionDeclarationNode node);

    void VisitFunctionType(FunctionTypeNode node);

    void VisitGenericType(GenericApplicationNode node);

    void VisitIfDirective(IfDirectiveNode node);

    void VisitIf(IfStatementNode node);

    void VisitInterface(InterfaceNode node);

    void VisitInterfaceProperty(InterfacePropertyNode node);

    void VisitInterfaceMethod(InterfaceMethodNode node);

    void VisitIsExpression(IsExpressionNode node);

    void VisitLiteral(LiteralExpressionNode node);

    void VisitMemberAccess(MemberAccessExpressionNode node);

    void VisitMethod(MethodDeclarationNode node);

    void VisitNamespace(NamespaceNode node);

    void VisitNewArray(NewArrayExpressionNode node);

    void VisitNewObject(NewObjectExpressionNode node);

    void VisitNull(NullExpressionNode node);

    void VisitParameter(ParameterNode node);

    void VisitProperty(PropertyDeclarationNode node);

    void VisitGetter(PropertyGetterNode node);

    void VisitSetter(PropertySetterNode node);

    void VisitReturn(ReturnStatementNode node);

    void VisitTree(SyntaxTree node);

    void VisitTuple(TupleExpressionNode node);

    void VisitTupleType(TupleTypeNode node);

    void VisitType(TypeDeclarationNode node);

    void VisitTypeNode(TypeRefNode node);

    void VisitUnaryExpression(UnaryExpressionNode node);

    void VisitUse(UseNode node);

    void VisitVariable(VariableDeclarationNode node);

    void VisitWhile(WhileNode node);
}