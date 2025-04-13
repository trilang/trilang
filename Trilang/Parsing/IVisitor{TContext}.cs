using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor<in TContext>
{
    void Visit(ArrayAccessExpressionNode node, TContext context);

    void Visit(BinaryExpressionNode node, TContext context);

    void Visit(BlockStatementNode node, TContext context);

    void Visit(BreakNode node, TContext context);

    void Visit(CallExpressionNode node, TContext context);

    void Visit(ConstructorDeclarationNode node, TContext context);

    void Visit(ContinueNode node, TContext context);

    void Visit(ExpressionStatementNode node, TContext context);

    void Visit(FieldDeclarationNode node, TContext context);

    void Visit(FunctionDeclarationNode node, TContext context);

    void Visit(FunctionTypeDeclarationNode node, TContext context);

    void Visit(IfStatementNode node, TContext context);

    void Visit(LiteralExpressionNode node, TContext context);

    void Visit(MemberAccessExpressionNode node, TContext context);

    void Visit(MethodDeclarationNode node, TContext context);

    void Visit(ParameterNode node, TContext context);

    void Visit(ReturnStatementNode node, TContext context);

    void Visit(SyntaxTree node, TContext context);

    void Visit(TypeAliasDeclarationNode node, TContext context);

    void Visit(TypeDeclarationNode node, TContext context);

    void Visit(TypeNode node, TContext context);

    void Visit(UnaryExpressionNode node, TContext context);

    void Visit(VariableDeclarationStatementNode node, TContext context);

    void Visit(WhileNode node, TContext context);
}