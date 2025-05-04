using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public interface IVisitor<in TContext>
{
    void Visit(ArrayAccessExpressionNode node, TContext context);

    void Visit(ArrayTypeNode node, TContext context);

    void Visit(BinaryExpressionNode node, TContext context);

    void Visit(BlockStatementNode node, TContext context);

    void Visit(BreakNode node, TContext context);

    void Visit(CallExpressionNode node, TContext context);

    void Visit(ConstructorDeclarationNode node, TContext context);

    void Visit(ContinueNode node, TContext context);

    void Visit(DiscriminatedUnionNode node, TContext context);

    void Visit(ExpressionStatementNode node, TContext context);

    void Visit(FieldDeclarationNode node, TContext context);

    void Visit(FunctionDeclarationNode node, TContext context);

    void Visit(FunctionTypeNode node, TContext context);

    void Visit(IfStatementNode node, TContext context);

    void Visit(InterfaceNode node, TContext context);

    void Visit(InterfaceFieldNode node, TContext context);

    void Visit(InterfaceMethodNode node, TContext context);

    void Visit(LiteralExpressionNode node, TContext context);

    void Visit(MemberAccessExpressionNode node, TContext context);

    void Visit(MethodDeclarationNode node, TContext context);

    void Visit(NewArrayExpressionNode node, TContext context);

    void Visit(NewObjectExpressionNode node, TContext context);

    void Visit(NullExpressionNode node, TContext context);

    void Visit(ParameterNode node, TContext context);

    void Visit(ReturnStatementNode node, TContext context);

    void Visit(SyntaxTree node, TContext context);

    void Visit(TupleExpressionNode node, TContext context);

    void Visit(TupleTypeNode node, TContext context);

    void Visit(TypeAliasDeclarationNode node, TContext context);

    void Visit(TypeDeclarationNode node, TContext context);

    void Visit(TypeNode node, TContext context);

    void Visit(UnaryExpressionNode node, TContext context);

    void Visit(VariableDeclarationStatementNode node, TContext context);

    void Visit(WhileNode node, TContext context);
}