using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor
{
    void Visit(ArrayAccessExpressionNode node);

    void Visit(BinaryExpressionNode node);

    void Visit(BlockStatementNode node);

    void Visit(BreakNode node);

    void Visit(CallExpressionNode node);

    void Visit(ConstructorDeclarationNode node);

    void Visit(ContinueNode node);

    void Visit(ExpressionStatementNode node);

    void Visit(FieldDeclarationNode node);

    void Visit(FunctionDeclarationNode node);

    void Visit(IfStatementNode node);

    void Visit(LiteralExpressionNode node);

    void Visit(MemberAccessExpressionNode node);

    void Visit(MethodDeclarationNode node);

    void Visit(ParameterNode node);

    void Visit(ReturnStatementNode node);

    void Visit(SyntaxTree node);

    void Visit(TypeAliasNode node);

    void Visit(TypeDeclarationNode node);

    void Visit(TypeNode node);

    void Visit(UnaryExpressionNode node);

    void Visit(VariableDeclarationStatementNode node);

    void Visit(WhileNode node);
}