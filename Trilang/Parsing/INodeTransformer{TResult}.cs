using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface INodeTransformer<out TResult>
{
    TResult TransformTypeAlias(AliasDeclarationNode node);

    TResult TransformArrayAccess(ArrayAccessExpressionNode node);

    TResult TransformArrayType(ArrayTypeNode node);

    TResult TransformBinaryExpression(BinaryExpressionNode node);

    TResult TransformBlock(BlockStatementNode node);

    TResult TransformBreak(BreakNode node);

    TResult TransformCall(CallExpressionNode node);

    TResult TransformCast(CastExpressionNode node);

    TResult TransformConstructor(ConstructorDeclarationNode node);

    TResult TransformContinue(ContinueNode node);

    TResult TransformDiscriminatedUnion(DiscriminatedUnionNode node);

    TResult TransformExpressionStatement(ExpressionStatementNode node);

    TResult TransformFakeDeclaration(FakeDeclarationNode node);

    TResult TransformFakeExpression(FakeExpressionNode node);

    TResult TransformFakeStatement(FakeStatementNode node);

    TResult TransformFakeType(FakeTypeNode node);

    TResult TransformFunction(FunctionDeclarationNode node);

    TResult TransformFunctionType(FunctionTypeNode node);

    TResult TransformGenericType(GenericTypeNode node);

    TResult TransformIfDirective(IfDirectiveNode node);

    TResult TransformIf(IfStatementNode node);

    TResult TransformInterface(InterfaceNode node);

    TResult TransformInterfaceProperty(InterfacePropertyNode node);

    TResult TransformInterfaceMethod(InterfaceMethodNode node);

    TResult TransformAsExpression(IsExpressionNode node);

    TResult TransformLiteral(LiteralExpressionNode node);

    TResult TransformMemberAccess(MemberAccessExpressionNode node);

    TResult TransformMethod(MethodDeclarationNode node);

    TResult TransformNamespace(NamespaceNode node);

    TResult TransformNewArray(NewArrayExpressionNode node);

    TResult TransformNewObject(NewObjectExpressionNode node);

    TResult TransformNull(NullExpressionNode node);

    TResult TransformParameter(ParameterNode node);

    TResult TransformProperty(PropertyDeclarationNode node);

    TResult TransformGetter(PropertyGetterNode node);

    TResult TransformSetter(PropertySetterNode node);

    TResult TransformReturn(ReturnStatementNode node);

    TResult TransformTree(SyntaxTree node);

    TResult TransformTuple(TupleExpressionNode node);

    TResult TransformTupleType(TupleTypeNode node);

    TResult TransformType(TypeDeclarationNode node);

    TResult TransformTypeNode(TypeNode node);

    TResult TransformUnaryExpression(UnaryExpressionNode node);

    TResult TransformUse(UseNode node);

    TResult TransformVariable(VariableDeclarationNode node);

    TResult TransformWhile(WhileNode node);
}