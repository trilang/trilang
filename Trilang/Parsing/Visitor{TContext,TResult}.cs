using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public abstract class Visitor<TContext, TResult> : IVisitor<TContext>
    where TContext : VisitorContext<TResult>
{
    public void Visit(ArrayAccessExpressionNode node, TContext context)
    {
        VisitEnter(node, context);

        node.Member.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ArrayAccessExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ArrayAccessExpressionNode node, TContext context)
    {
    }

    public void Visit(ArrayTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.ElementType.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ArrayTypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ArrayTypeNode node, TContext context)
    {
    }

    public void Visit(BinaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BinaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BinaryExpressionNode node, TContext context)
    {
    }

    public void Visit(BlockStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BlockStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BlockStatementNode node, TContext context)
    {
    }

    public void Visit(BreakNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(BreakNode node, TContext context)
    {
    }

    protected virtual void VisitExit(BreakNode node, TContext context)
    {
    }

    public void Visit(CallExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(CallExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(CallExpressionNode node, TContext context)
    {
    }

    public void Visit(ConstructorDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.Body.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ConstructorDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ConstructorDeclarationNode node, TContext context)
    {
    }

    public void Visit(ContinueNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ContinueNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ContinueNode node, TContext context)
    {
    }

    public void Visit(DiscriminatedUnionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(DiscriminatedUnionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(DiscriminatedUnionNode node, TContext context)
    {
    }

    public void Visit(ExpressionStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Expression.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ExpressionStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ExpressionStatementNode node, TContext context)
    {
    }

    public void Visit(FunctionDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(FunctionDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(FunctionDeclarationNode node, TContext context)
    {
    }

    public void Visit(FunctionTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(FunctionTypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(FunctionTypeNode node, TContext context)
    {
    }

    public void Visit(GenericTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var type in node.TypeArguments)
            type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(GenericTypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(GenericTypeNode node, TContext context)
    {
    }

    public void Visit(IfStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(IfStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(IfStatementNode node, TContext context)
    {
    }

    public void Visit(InterfaceNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(InterfaceNode node, TContext context)
    {
    }

    protected virtual void VisitExit(InterfaceNode node, TContext context)
    {
    }

    public void Visit(InterfacePropertyNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(InterfacePropertyNode node, TContext context)
    {
    }

    protected virtual void VisitExit(InterfacePropertyNode node, TContext context)
    {
    }

    public void Visit(InterfaceMethodNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(InterfaceMethodNode node, TContext context)
    {
    }

    protected virtual void VisitExit(InterfaceMethodNode node, TContext context)
    {
    }

    public void Visit(LiteralExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(LiteralExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(LiteralExpressionNode node, TContext context)
    {
    }

    public void Visit(MemberAccessExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Member?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(MemberAccessExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(MemberAccessExpressionNode node, TContext context)
    {
    }

    public void Visit(MethodDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);
        node.Body.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(MethodDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(MethodDeclarationNode node, TContext context)
    {
    }

    public void Visit(NewArrayExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(NewArrayExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(NewArrayExpressionNode node, TContext context)
    {
    }

    public void Visit(NewObjectExpressionNode node, TContext context)
    {
        VisitEnter(node, context);

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(NewObjectExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(NewObjectExpressionNode node, TContext context)
    {
    }

    public void Visit(NullExpressionNode node, TContext context)
    {
        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(NullExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(NullExpressionNode node, TContext context)
    {
    }

    public void Visit(ReturnStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Expression?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ReturnStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ReturnStatementNode node, TContext context)
    {
    }

    public void Visit(ParameterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(ParameterNode node, TContext context)
    {
    }

    protected virtual void VisitExit(ParameterNode node, TContext context)
    {
    }

    public void Visit(PropertyDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(PropertyDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(PropertyDeclarationNode node, TContext context)
    {
    }

    public void Visit(PropertyGetterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Body?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(PropertyGetterNode node, TContext context)
    {
    }

    protected virtual void VisitExit(PropertyGetterNode node, TContext context)
    {
    }

    public void Visit(PropertySetterNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Body?.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(PropertySetterNode node, TContext context)
    {
    }

    protected virtual void VisitExit(PropertySetterNode node, TContext context)
    {
    }

    public void Visit(SyntaxTree node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var function in node.Declarations)
            function.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(SyntaxTree node, TContext context)
    {
    }

    protected virtual void VisitExit(SyntaxTree node, TContext context)
    {
    }

    public void Visit(TupleExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var element in node.Expressions)
            element.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TupleExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TupleExpressionNode node, TContext context)
    {
    }

    public void Visit(TupleTypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TupleTypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TupleTypeNode node, TContext context)
    {
    }


    public void Visit(TypeAliasDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TypeAliasDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TypeAliasDeclarationNode node, TContext context)
    {
    }

    public void Visit(TypeDeclarationNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TypeDeclarationNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TypeDeclarationNode node, TContext context)
    {
    }

    public void Visit(TypeNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);
        VisitExit(node, context);
    }

    protected virtual void VisitEnter(TypeNode node, TContext context)
    {
    }

    protected virtual void VisitExit(TypeNode node, TContext context)
    {
    }

    public void Visit(UnaryExpressionNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Operand.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(UnaryExpressionNode node, TContext context)
    {
    }

    protected virtual void VisitExit(UnaryExpressionNode node, TContext context)
    {
    }

    public void Visit(VariableDeclarationStatementNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(VariableDeclarationStatementNode node, TContext context)
    {
    }

    protected virtual void VisitExit(VariableDeclarationStatementNode node, TContext context)
    {
    }

    public void Visit(WhileNode node, TContext context)
    {
        if (context.IsFinished)
            return;

        VisitEnter(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);

        VisitExit(node, context);
    }

    protected virtual void VisitEnter(WhileNode node, TContext context)
    {
    }

    protected virtual void VisitExit(WhileNode node, TContext context)
    {
    }
}