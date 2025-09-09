using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics;

public interface ITransformer<out TResult>
{
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

    TResult TransformFunction(FunctionDeclaration node);

    TResult TransformFunctionType(FunctionType node);

    TResult TransformGenericType(GenericType node);

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

    TResult TransformTypeAlias(TypeAliasDeclaration node);

    TResult TransformType(TypeDeclaration node);

    TResult TransformTypeNode(Type node);

    TResult TransformUnaryExpression(UnaryExpression node);

    TResult TransformVariable(VariableDeclaration node);

    TResult TransformWhile(While node);
}