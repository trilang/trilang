using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor
{
    void VisitArrayAccess(ArrayAccessExpressionNode node);

    void VisitArrayType(ArrayTypeNode node);

    void VisitAsExpression(AsExpressionNode node);

    void VisitBinaryExpression(BinaryExpressionNode node);

    void VisitBlock(BlockStatementNode node);

    void VisitBreak(BreakNode node);

    void VisitCall(CallExpressionNode node);

    void VisitConstructor(ConstructorDeclarationNode node);

    void VisitContinue(ContinueNode node);

    void VisitDiscriminatedUnion(DiscriminatedUnionNode node);

    void VisitExpressionBlock(ExpressionBlockNode node);

    void VisitExpressionStatement(ExpressionStatementNode node);

    void VisitFunction(FunctionDeclarationNode node);

    void VisitFunctionType(FunctionTypeNode node);

    void VisitGenericType(GenericTypeNode node);

    void VisitGoTo(GoToNode node);

    void VisitIfDirective(IfDirectiveNode node);

    void VisitIf(IfStatementNode node);

    void VisitInterface(InterfaceNode node);

    void VisitInterfaceProperty(InterfacePropertyNode node);

    void VisitInterfaceMethod(InterfaceMethodNode node);

    void VisitLabel(LabelNode node);

    void VisitLiteral(LiteralExpressionNode node);

    void VisitMemberAccess(MemberAccessExpressionNode node);

    void VisitMethod(MethodDeclarationNode node);

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

    void VisitTypeAlias(TypeAliasDeclarationNode node);

    void VisitType(TypeDeclarationNode node);

    void VisitTypeNode(TypeNode node);

    void VisitUnaryExpression(UnaryExpressionNode node);

    void VisitVariable(VariableDeclarationStatementNode node);

    void VisitWhile(WhileNode node);
}