using Trilang.Semantics.Model;

namespace Trilang.Semantics;

public interface ITransformer<out TResult>
{
    TResult TransformTypeAlias(AliasDeclaration node);

    TResult TransformArrayAccess(ArrayAccessExpression node);

    TResult TransformArrayType(ArrayType node);

    TResult TransformBinaryExpression(BinaryExpression node);

    TResult TransformBlock(BlockStatement node);

    TResult TransformBreak(Break node);

    TResult TransformCall(CallExpression node);

    TResult TransformCast(CastExpression node);

    TResult TransformConstructor(ConstructorDeclaration node);

    TResult TransformContinue(Continue node);

    TResult TransformDiscriminatedUnion(DiscriminatedUnion node);

    TResult TransformExpressionBlock(ExpressionBlock node);

    TResult TransformExpressionStatement(ExpressionStatement node);

    TResult TransformFakeDeclaration(FakeDeclaration node);

    TResult TransformFakeExpression(FakeExpression node);

    TResult TransformFakeStatement(FakeStatement node);

    TResult TransformFakeType(FakeType node);

    TResult TransformFunction(FunctionDeclaration node);

    TResult TransformFunctionType(FunctionType node);

    TResult TransformGenericType(GenericTypeRef node);

    TResult TransformGoTo(GoTo node);

    TResult TransformIfDirective(IfDirective node);

    TResult TransformIf(IfStatement node);

    TResult TransformInterface(Interface node);

    TResult TransformInterfaceProperty(InterfaceProperty node);

    TResult TransformInterfaceMethod(InterfaceMethod node);

    TResult TransformAsExpression(IsExpression node);

    TResult TransformLabel(Label node);

    TResult TransformLiteral(LiteralExpression node);

    TResult TransformMemberAccess(MemberAccessExpression node);

    TResult TransformMethod(MethodDeclaration node);

    TResult TransformNamespace(Namespace node);

    TResult TransformNewArray(NewArrayExpression node);

    TResult TransformNewObject(NewObjectExpression node);

    TResult TransformNull(NullExpression node);

    TResult TransformParameter(Parameter node);

    TResult TransformProperty(PropertyDeclaration node);

    TResult TransformGetter(PropertyGetter node);

    TResult TransformSetter(PropertySetter node);

    TResult TransformReturn(ReturnStatement node);

    TResult TransformTree(SemanticTree node);

    TResult TransformTuple(TupleExpression node);

    TResult TransformTupleType(TupleType node);

    TResult TransformType(TypeDeclaration node);

    TResult TransformTypeNode(TypeRef node);

    TResult TransformUnaryExpression(UnaryExpression node);

    TResult TransformUse(Use node);

    TResult TransformVariable(VariableDeclaration node);

    TResult TransformWhile(While node);
}