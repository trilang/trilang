using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics;

public interface IVisitor
{
    void VisitTypeAlias(AliasDeclaration node);

    void VisitArrayAccess(ArrayAccessExpression node);

    void VisitArrayType(ArrayType node);

    void VisitBinaryExpression(BinaryExpression node);

    void VisitBlock(BlockStatement node);

    void VisitBreak(Break node);

    void VisitCall(CallExpression node);

    void VisitCast(CastExpression node);

    void VisitConstructor(ConstructorDeclaration node);

    void VisitContinue(Continue node);

    void VisitDiscriminatedUnion(DiscriminatedUnion node);

    void VisitExpressionBlock(ExpressionBlock node);

    void VisitExpressionStatement(ExpressionStatement node);

    void VisitFakeDeclaration(FakeDeclaration node);

    void VisitFakeExpression(FakeExpression node);

    void VisitFakeStatement(FakeStatement node);

    void VisitFakeType(FakeType node);

    void VisitFunction(FunctionDeclaration node);

    void VisitFunctionType(FunctionType node);

    void VisitGenericType(GenericType node);

    void VisitGoTo(GoTo node);

    void VisitIfDirective(IfDirective node);

    void VisitIf(IfStatement node);

    void VisitInterface(Interface node);

    void VisitInterfaceProperty(InterfaceProperty node);

    void VisitInterfaceMethod(InterfaceMethod node);

    void VisitIsExpression(IsExpression node);

    void VisitLabel(Label node);

    void VisitLiteral(LiteralExpression node);

    void VisitMemberAccess(MemberAccessExpression node);

    void VisitMethod(MethodDeclaration node);

    void VisitNamespace(Namespace node);

    void VisitNewArray(NewArrayExpression node);

    void VisitNewObject(NewObjectExpression node);

    void VisitNull(NullExpression node);

    void VisitParameter(Parameter node);

    void VisitProperty(PropertyDeclaration node);

    void VisitGetter(PropertyGetter node);

    void VisitSetter(PropertySetter node);

    void VisitReturn(ReturnStatement node);

    void VisitTree(SemanticTree node);

    void VisitTuple(TupleExpression node);

    void VisitTupleType(TupleType node);

    void VisitType(TypeDeclaration node);

    void VisitTypeNode(Type node);

    void VisitUnaryExpression(UnaryExpression node);

    void VisitUse(Use node);

    void VisitVariable(VariableDeclaration node);

    void VisitWhile(While node);
}