using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor : IVisitor
{
    public void Visit(ArrayAccessExpressionNode node)
    {
        VisitEnter(node);

        node.Member.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ArrayAccessExpressionNode node)
    {
    }

    protected virtual void VisitExit(ArrayAccessExpressionNode node)
    {
    }

    public void Visit(ArrayTypeNode node)
    {
        VisitEnter(node);

        node.ElementType.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ArrayTypeNode node)
    {
    }

    protected virtual void VisitExit(ArrayTypeNode node)
    {
    }

    public void Visit(BinaryExpressionNode node)
    {
        VisitEnter(node);

        node.Left.Accept(this);
        node.Right.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(BinaryExpressionNode node)
    {
    }

    protected virtual void VisitExit(BinaryExpressionNode node)
    {
    }

    public void Visit(BlockStatementNode node)
    {
        VisitEnter(node);

        foreach (var statement in node.Statements)
            statement.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(BlockStatementNode node)
    {
    }

    protected virtual void VisitExit(BlockStatementNode node)
    {
    }

    public void Visit(BreakNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(BreakNode node)
    {
    }

    protected virtual void VisitExit(BreakNode node)
    {
    }

    public void Visit(CallExpressionNode node)
    {
        VisitEnter(node);

        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(CallExpressionNode node)
    {
    }

    protected virtual void VisitExit(CallExpressionNode node)
    {
    }

    public void Visit(ConstructorDeclarationNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.Body.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ConstructorDeclarationNode node)
    {
    }

    protected virtual void VisitExit(ConstructorDeclarationNode node)
    {
    }

    public void Visit(ContinueNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(ContinueNode node)
    {
    }

    protected virtual void VisitExit(ContinueNode node)
    {
    }

    public void Visit(DiscriminatedUnionNode node)
    {
        VisitEnter(node);

        foreach (var type in node.Types)
            type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(DiscriminatedUnionNode node)
    {
    }

    protected virtual void VisitExit(DiscriminatedUnionNode node)
    {
    }

    public void Visit(ExpressionStatementNode node)
    {
        VisitEnter(node);

        node.Expression.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ExpressionStatementNode node)
    {
    }

    protected virtual void VisitExit(ExpressionStatementNode node)
    {
    }

    public void Visit(FunctionDeclarationNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);
        node.Body?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(FunctionDeclarationNode node)
    {
    }

    protected virtual void VisitExit(FunctionDeclarationNode node)
    {
    }

    public void Visit(FunctionTypeNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(FunctionTypeNode node)
    {
    }

    protected virtual void VisitExit(FunctionTypeNode node)
    {
    }

    public void Visit(GenericTypeNode node)
    {
        VisitEnter(node);

        foreach (var type in node.TypeArguments)
            type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(GenericTypeNode node)
    {
    }

    protected virtual void VisitExit(GenericTypeNode node)
    {
    }

    public void Visit(IfDirectiveNode node)
    {
        VisitEnter(node);

        foreach (var then in node.Then)
            then.Accept(this);

        foreach (var @else in node.Else)
            @else.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(IfDirectiveNode node)
    {
    }

    protected virtual void VisitExit(IfDirectiveNode node)
    {
    }

    public void Visit(IfStatementNode node)
    {
        VisitEnter(node);

        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(IfStatementNode node)
    {
    }

    protected virtual void VisitExit(IfStatementNode node)
    {
    }

    public void Visit(InterfaceNode node)
    {
        VisitEnter(node);

        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(InterfaceNode node)
    {
    }

    protected virtual void VisitExit(InterfaceNode node)
    {
    }

    public void Visit(InterfacePropertyNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(InterfacePropertyNode node)
    {
    }

    protected virtual void VisitExit(InterfacePropertyNode node)
    {
    }

    public void Visit(InterfaceMethodNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(InterfaceMethodNode node)
    {
    }

    protected virtual void VisitExit(InterfaceMethodNode node)
    {
    }

    public void Visit(LiteralExpressionNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(LiteralExpressionNode node)
    {
    }

    protected virtual void VisitExit(LiteralExpressionNode node)
    {
    }

    public void Visit(MemberAccessExpressionNode node)
    {
        VisitEnter(node);

        node.Member?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(MemberAccessExpressionNode node)
    {
    }

    protected virtual void VisitExit(MemberAccessExpressionNode node)
    {
    }

    public void Visit(MethodDeclarationNode node)
    {
        VisitEnter(node);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);
        node.Body.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(MethodDeclarationNode node)
    {
    }

    protected virtual void VisitExit(MethodDeclarationNode node)
    {
    }

    public void Visit(NewArrayExpressionNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);
        node.Size.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(NewArrayExpressionNode node)
    {
    }

    protected virtual void VisitExit(NewArrayExpressionNode node)
    {
    }

    public void Visit(NewObjectExpressionNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(NewObjectExpressionNode node)
    {
    }

    protected virtual void VisitExit(NewObjectExpressionNode node)
    {
    }

    public void Visit(NullExpressionNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(NullExpressionNode node)
    {
    }

    protected virtual void VisitExit(NullExpressionNode node)
    {
    }

    public void Visit(ReturnStatementNode node)
    {
        VisitEnter(node);

        node.Expression?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ReturnStatementNode node)
    {
    }

    protected virtual void VisitExit(ReturnStatementNode node)
    {
    }

    public void Visit(ParameterNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(ParameterNode node)
    {
    }

    protected virtual void VisitExit(ParameterNode node)
    {
    }

    public void Visit(PropertyDeclarationNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);
        node.Getter?.Accept(this);
        node.Setter?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(PropertyDeclarationNode node)
    {
    }

    protected virtual void VisitExit(PropertyDeclarationNode node)
    {
    }

    public void Visit(PropertyGetterNode node)
    {
        VisitEnter(node);

        node.Body?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(PropertyGetterNode node)
    {
    }

    protected virtual void VisitExit(PropertyGetterNode node)
    {
    }

    public void Visit(PropertySetterNode node)
    {
        VisitEnter(node);

        node.Body?.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(PropertySetterNode node)
    {
    }

    protected virtual void VisitExit(PropertySetterNode node)
    {
    }

    public void Visit(SyntaxTree node)
    {
        VisitEnter(node);

        foreach (var function in node.Declarations)
            function.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(SyntaxTree node)
    {
    }

    protected virtual void VisitExit(SyntaxTree node)
    {
    }

    public void Visit(TupleExpressionNode node)
    {
        VisitEnter(node);

        foreach (var element in node.Expressions)
            element.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(TupleExpressionNode node)
    {
    }

    protected virtual void VisitExit(TupleExpressionNode node)
    {
    }

    public void Visit(TupleTypeNode node)
    {
        VisitEnter(node);

        foreach (var type in node.Types)
            type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(TupleTypeNode node)
    {
    }

    protected virtual void VisitExit(TupleTypeNode node)
    {
    }

    public void Visit(TypeAliasDeclarationNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(TypeAliasDeclarationNode node)
    {
    }

    protected virtual void VisitExit(TypeAliasDeclarationNode node)
    {
    }

    public void Visit(TypeDeclarationNode node)
    {
        VisitEnter(node);

        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(TypeDeclarationNode node)
    {
    }

    protected virtual void VisitExit(TypeDeclarationNode node)
    {
    }

    public void Visit(TypeNode node)
    {
        VisitEnter(node);
        VisitExit(node);
    }

    protected virtual void VisitEnter(TypeNode node)
    {
    }

    protected virtual void VisitExit(TypeNode node)
    {
    }

    public void Visit(UnaryExpressionNode node)
    {
        VisitEnter(node);

        node.Operand.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(UnaryExpressionNode node)
    {
    }

    protected virtual void VisitExit(UnaryExpressionNode node)
    {
    }

    public void Visit(VariableDeclarationStatementNode node)
    {
        VisitEnter(node);

        node.Type.Accept(this);
        node.Expression.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(VariableDeclarationStatementNode node)
    {
    }

    protected virtual void VisitExit(VariableDeclarationStatementNode node)
    {
    }

    public void Visit(WhileNode node)
    {
        VisitEnter(node);

        node.Condition.Accept(this);
        node.Body.Accept(this);

        VisitExit(node);
    }

    protected virtual void VisitEnter(WhileNode node)
    {
    }

    protected virtual void VisitExit(WhileNode node)
    {
    }
}