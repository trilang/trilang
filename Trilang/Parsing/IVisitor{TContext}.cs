using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor<in TContext>
{
    void VisitArrayAccess(ArrayAccessExpressionNode node, TContext context);

    void VisitArrayType(ArrayTypeNode node, TContext context);

    void VisitAsExpression(AsExpressionNode node, TContext context);

    void VisitBinaryExpression(BinaryExpressionNode node, TContext context);

    void VisitBlock(BlockStatementNode node, TContext context);

    void VisitBreak(BreakNode node, TContext context);

    void VisitCall(CallExpressionNode node, TContext context);

    void VisitConstructor(ConstructorDeclarationNode node, TContext context);

    void VisitContinue(ContinueNode node, TContext context);

    void VisitDiscriminatedUnion(DiscriminatedUnionNode node, TContext context);

    void VisitExpressionStatement(ExpressionStatementNode node, TContext context);

    void VisitFunction(FunctionDeclarationNode node, TContext context);

    void VisitFunctionType(FunctionTypeNode node, TContext context);

    void VisitGenericType(GenericTypeNode node, TContext context);

    void VisitGoTo(GoToNode node, TContext context);

    void VisitIfDirective(IfDirectiveNode node, TContext context);

    void VisitIf(IfStatementNode node, TContext context);

    void VisitInterface(InterfaceNode node, TContext context);

    void VisitInterfaceProperty(InterfacePropertyNode node, TContext context);

    void VisitInterfaceMethod(InterfaceMethodNode node, TContext context);

    void VisitLabel(LabelNode node, TContext context);

    void VisitLiteral(LiteralExpressionNode node, TContext context);

    void VisitMemberAccess(MemberAccessExpressionNode node, TContext context);

    void VisitMethod(MethodDeclarationNode node, TContext context);

    void VisitNewArray(NewArrayExpressionNode node, TContext context);

    void VisitNewObject(NewObjectExpressionNode node, TContext context);

    void VisitNull(NullExpressionNode node, TContext context);

    void VisitParameter(ParameterNode node, TContext context);

    void VisitProperty(PropertyDeclarationNode node, TContext context);

    void VisitGetter(PropertyGetterNode node, TContext context);

    void VisitSetter(PropertySetterNode node, TContext context);

    void VisitReturn(ReturnStatementNode node, TContext context);

    void VisitTree(SyntaxTree node, TContext context);

    void VisitTuple(TupleExpressionNode node, TContext context);

    void VisitTupleType(TupleTypeNode node, TContext context);

    void VisitTypeAlias(TypeAliasDeclarationNode node, TContext context);

    void VisitType(TypeDeclarationNode node, TContext context);

    void VisitTypeNode(TypeNode node, TContext context);

    void VisitUnaryExpression(UnaryExpressionNode node, TContext context);

    void VisitVariable(VariableDeclarationStatementNode node, TContext context);

    void VisitWhile(WhileNode node, TContext context);
}