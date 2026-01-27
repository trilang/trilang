using Trilang.Semantics.Model;

namespace Trilang.Semantics;

public interface IVisitor<in TContext>
{
    void VisitAlias(AliasDeclaration node, TContext context);

    void VisitArrayAccess(ArrayAccessExpression node, TContext context);

    void VisitArrayType(ArrayType node, TContext context);

    void VisitBinaryExpression(BinaryExpression node, TContext context);

    void VisitBlock(BlockStatement node, TContext context);

    void VisitBreak(Break node, TContext context);

    void VisitCall(CallExpression node, TContext context);

    void VisitCast(CastExpression node, TContext context);

    void VisitConstructor(ConstructorDeclaration node, TContext context);

    void VisitContinue(Continue node, TContext context);

    void VisitDiscriminatedUnion(DiscriminatedUnion node, TContext context);

    void VisitExpressionBlock(ExpressionBlock node, TContext context);

    void VisitExpressionStatement(ExpressionStatement node, TContext context);

    void VisitFakeDeclaration(FakeDeclaration node, TContext context);

    void VisitFakeExpression(FakeExpression node, TContext context);

    void VisitFakeStatement(FakeStatement node, TContext context);

    void VisitFakeType(FakeType node, TContext context);

    void VisitFunction(FunctionDeclaration node, TContext context);

    void VisitFunctionType(FunctionType node, TContext context);

    void VisitGenericType(GenericApplication node, TContext context);

    void VisitGoTo(GoTo node, TContext context);

    void VisitIfDirective(IfDirective node, TContext context);

    void VisitIf(IfStatement node, TContext context);

    void VisitInterface(Interface node, TContext context);

    void VisitInterfaceProperty(InterfaceProperty node, TContext context);

    void VisitInterfaceMethod(InterfaceMethod node, TContext context);

    void VisitIsExpression(IsExpression node, TContext context);

    void VisitLabel(Label node, TContext context);

    void VisitLiteral(LiteralExpression node, TContext context);

    void VisitMemberAccess(MemberAccessExpression node, TContext context);

    void VisitMethod(MethodDeclaration node, TContext context);

    void VisitNamespace(Namespace node, TContext context);

    void VisitNewArray(NewArrayExpression node, TContext context);

    void VisitNewObject(NewObjectExpression node, TContext context);

    void VisitNull(NullExpression node, TContext context);

    void VisitParameter(Parameter node, TContext context);

    void VisitProperty(PropertyDeclaration node, TContext context);

    void VisitGetter(PropertyGetter node, TContext context);

    void VisitSetter(PropertySetter node, TContext context);

    void VisitReturn(ReturnStatement node, TContext context);

    void VisitTree(SemanticTree node, TContext context);

    void VisitTuple(TupleExpression node, TContext context);

    void VisitTupleType(TupleType node, TContext context);

    void VisitType(TypeDeclaration node, TContext context);

    void VisitTypeNode(TypeRef node, TContext context);

    void VisitUnaryExpression(UnaryExpression node, TContext context);

    void VisitUse(Use node, TContext context);

    void VisitVariable(VariableDeclaration node, TContext context);

    void VisitWhile(While node, TContext context);
}