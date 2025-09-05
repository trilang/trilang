using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface ITransformer<out T>
{
    T TransformArrayAccess(ArrayAccessExpressionNode node);

    T TransformArrayType(ArrayTypeNode node);

    T TransformBinaryExpression(BinaryExpressionNode node);

    T TransformBlock(BlockStatementNode node);

    T TransformBreak(BreakNode node);

    T TransformCall(CallExpressionNode node);

    T TransformCast(CastExpressionNode node);

    T TransformConstructor(ConstructorDeclarationNode node);

    T TransformContinue(ContinueNode node);

    T TransformDiscriminatedUnion(DiscriminatedUnionNode node);

    T TransformExpressionBlock(ExpressionBlockNode node);

    T TransformExpressionStatement(ExpressionStatementNode node);

    T TransformFunction(FunctionDeclarationNode node);

    T TransformFunctionType(FunctionTypeNode node);

    T TransformGenericType(GenericTypeNode node);

    T TransformGoTo(GoToNode node);

    T TransformIfDirective(IfDirectiveNode node);

    T TransformIf(IfStatementNode node);

    T TransformInterface(InterfaceNode node);

    T TransformInterfaceProperty(InterfacePropertyNode node);

    T TransformInterfaceMethod(InterfaceMethodNode node);

    T TransformAsExpression(IsExpressionNode node);

    T TransformLabel(LabelNode node);

    T TransformLiteral(LiteralExpressionNode node);

    T TransformMemberAccess(MemberAccessExpressionNode node);

    T TransformMethod(MethodDeclarationNode node);

    T TransformNewArray(NewArrayExpressionNode node);

    T TransformNewObject(NewObjectExpressionNode node);

    T TransformNull(NullExpressionNode node);

    T TransformParameter(ParameterNode node);

    T TransformProperty(PropertyDeclarationNode node);

    T TransformGetter(PropertyGetterNode node);

    T TransformSetter(PropertySetterNode node);

    T TransformReturn(ReturnStatementNode node);

    T TransformTree(SyntaxTree node);

    T TransformTuple(TupleExpressionNode node);

    T TransformTupleType(TupleTypeNode node);

    T TransformTypeAlias(TypeAliasDeclarationNode node);

    T TransformType(TypeDeclarationNode node);

    T TransformTypeNode(TypeNode node);

    T TransformUnaryExpression(UnaryExpressionNode node);

    T TransformVariable(VariableDeclarationStatementNode node);

    T TransformWhile(WhileNode node);
}