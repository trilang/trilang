using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor
{
    void Visit(ArrayAccessExpressionNode node);

    void Visit(ArrayTypeNode node);

    void Visit(BinaryExpressionNode node);

    void Visit(BlockStatementNode node);

    void Visit(BreakNode node);

    void Visit(CallExpressionNode node);

    void Visit(ConstructorDeclarationNode node);

    void Visit(ContinueNode node);

    void Visit(DiscriminatedUnionNode node);

    void Visit(ExpressionStatementNode node);

    void Visit(FunctionDeclarationNode node);

    void Visit(FunctionTypeNode node);

    void Visit(GenericTypeNode node);

    void Visit(IfDirectiveNode node);

    void Visit(IfStatementNode node);

    void Visit(InterfaceNode node);

    void Visit(InterfacePropertyNode node);

    void Visit(InterfaceMethodNode node);

    void Visit(LiteralExpressionNode node);

    void Visit(MemberAccessExpressionNode node);

    void Visit(MethodDeclarationNode node);

    void Visit(NewArrayExpressionNode node);

    void Visit(NewObjectExpressionNode node);

    void Visit(NullExpressionNode node);

    void Visit(ParameterNode node);

    void Visit(PropertyDeclarationNode node);

    void Visit(PropertyGetterNode node);

    void Visit(PropertySetterNode node);

    void Visit(ReturnStatementNode node);

    void Visit(SyntaxTree node);

    void Visit(TupleExpressionNode node);

    void Visit(TupleTypeNode node);

    void Visit(TypeAliasDeclarationNode node);

    void Visit(TypeDeclarationNode node);

    void Visit(TypeNode node);

    void Visit(UnaryExpressionNode node);

    void Visit(VariableDeclarationStatementNode node);

    void Visit(WhileNode node);
}