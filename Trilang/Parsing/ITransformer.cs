using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface ITransformer
{
    ISyntaxNode TransformArrayAccess(ArrayAccessExpressionNode node);

    ISyntaxNode TransformArrayType(ArrayTypeNode node);

    ISyntaxNode TransformBinaryExpression(BinaryExpressionNode node);

    ISyntaxNode TransformBlock(BlockStatementNode node);

    ISyntaxNode TransformBreak(BreakNode node);

    ISyntaxNode TransformCall(CallExpressionNode node);

    ISyntaxNode TransformCast(CastExpressionNode node);

    ISyntaxNode TransformConstructor(ConstructorDeclarationNode node);

    ISyntaxNode TransformContinue(ContinueNode node);

    ISyntaxNode TransformDiscriminatedUnion(DiscriminatedUnionNode node);

    ISyntaxNode TransformExpressionBlock(ExpressionBlockNode node);

    ISyntaxNode TransformExpressionStatement(ExpressionStatementNode node);

    ISyntaxNode TransformFunction(FunctionDeclarationNode node);

    ISyntaxNode TransformFunctionType(FunctionTypeNode node);

    ISyntaxNode TransformGenericType(GenericTypeNode node);

    ISyntaxNode TransformGoTo(GoToNode node);

    ISyntaxNode TransformIfDirective(IfDirectiveNode node);

    ISyntaxNode TransformIf(IfStatementNode node);

    ISyntaxNode TransformInterface(InterfaceNode node);

    ISyntaxNode TransformInterfaceProperty(InterfacePropertyNode node);

    ISyntaxNode TransformInterfaceMethod(InterfaceMethodNode node);

    ISyntaxNode TransformAsExpression(IsExpressionNode node);

    ISyntaxNode TransformLabel(LabelNode node);

    ISyntaxNode TransformLiteral(LiteralExpressionNode node);

    ISyntaxNode TransformMemberAccess(MemberAccessExpressionNode node);

    ISyntaxNode TransformMethod(MethodDeclarationNode node);

    ISyntaxNode TransformNewArray(NewArrayExpressionNode node);

    ISyntaxNode TransformNewObject(NewObjectExpressionNode node);

    ISyntaxNode TransformNull(NullExpressionNode node);

    ISyntaxNode TransformParameter(ParameterNode node);

    ISyntaxNode TransformProperty(PropertyDeclarationNode node);

    ISyntaxNode TransformGetter(PropertyGetterNode node);

    ISyntaxNode TransformSetter(PropertySetterNode node);

    ISyntaxNode TransformReturn(ReturnStatementNode node);

    ISyntaxNode TransformTree(SyntaxTree node);

    ISyntaxNode TransformTuple(TupleExpressionNode node);

    ISyntaxNode TransformTupleType(TupleTypeNode node);

    ISyntaxNode TransformTypeAlias(TypeAliasDeclarationNode node);

    ISyntaxNode TransformType(TypeDeclarationNode node);

    ISyntaxNode TransformTypeNode(TypeNode node);

    ISyntaxNode TransformUnaryExpression(UnaryExpressionNode node);

    ISyntaxNode TransformVariable(VariableDeclarationStatementNode node);

    ISyntaxNode TransformWhile(WhileNode node);
}