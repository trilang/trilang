using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface INodeTransformer<out TResult>
{
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

    TResult TransformTypeAlias(TypeAliasDeclarationNode node);

    TResult TransformType(TypeDeclarationNode node);

    TResult TransformTypeNode(TypeNode node);

    TResult TransformUnaryExpression(UnaryExpressionNode node);

    TResult TransformVariable(VariableDeclarationNode node);

    TResult TransformWhile(WhileNode node);
}