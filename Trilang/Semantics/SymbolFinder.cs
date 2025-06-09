using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

internal class SymbolFinder : IVisitor<SymbolFinderContext>
{
    public void VisitArrayAccess(ArrayAccessExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

    public void VisitArrayType(ArrayTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.SymbolTable.TryAddType(TypeSymbol.Array(node));

        node.ElementType.Accept(this, context);
    }

    public void VisitAsExpression(AsExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);
    }

    public void VisitBinaryExpression(BinaryExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    public void VisitBlock(BlockStatementNode node, SymbolFinderContext context)
        => context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var statement in node.Statements)
                statement.Accept(this, c);
        });

    public void VisitBreak(BreakNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void VisitCall(CallExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitConstructor(ConstructorDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            c.SymbolTable.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void VisitContinue(ContinueNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node, SymbolFinderContext context)
    {
        var symbol = TypeSymbol.DiscriminatedUnion(node);
        context.SymbolTable.TryAddType(symbol);

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var type in node.Types)
                type.Accept(this, c);
        });
    }

    public void VisitExpressionStatement(ExpressionStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression.Accept(this, context);
    }

    public void VisitFunction(FunctionDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' function is already defined.");

        node.ReturnType.Accept(this, context);

        context.Scoped(c =>
        {
            c.DisableNextScope();

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body?.Accept(this, c);
        });
    }

    public void VisitFunctionType(FunctionTypeNode node, SymbolFinderContext context)
    {
        var symbol = TypeSymbol.FunctionType(node);
        context.SymbolTable.TryAddType(symbol);

        node.SymbolTable = context.SymbolTable;

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void VisitGenericType(GenericTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);

        var symbol = TypeSymbol.GenericType(node);
        node.SymbolTable.TryAddType(symbol);
    }

    public void VisitIfDirective(IfDirectiveNode node, SymbolFinderContext context)
    {
        if (!context.SemanticAnalysisOptions.HasDirective(node.DirectiveName))
            return;

        foreach (var then in node.Then)
            then.Accept(this, context);

        foreach (var @else in node.Else)
            @else.Accept(this, context);
    }

    public void VisitIf(IfStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void VisitInterface(InterfaceNode node, SymbolFinderContext context)
    {
        var symbol = TypeSymbol.Interface(node);
        context.SymbolTable.TryAddType(symbol);

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var property in node.Properties)
                property.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void VisitInterfaceProperty(InterfacePropertyNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");
    }

    public void VisitInterfaceMethod(InterfaceMethodNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        context.Scoped(c =>
        {
            foreach (var parameter in node.ParameterTypes)
                parameter.Accept(this, c);

            node.ReturnType.Accept(this, c);
        });
    }

    public void VisitLiteral(LiteralExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void VisitMemberAccess(MemberAccessExpressionNode node, SymbolFinderContext context)
    {
        node.Member?.Accept(this, context);

        node.SymbolTable = context.SymbolTable;
    }

    public void VisitMethod(MethodDeclarationNode node, SymbolFinderContext context)
    {
        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        node.SymbolTable = context.SymbolTable;

        node.ReturnType.Accept(this, context);

        context.Scoped(c =>
        {
            c.DisableNextScope();

            c.SymbolTable.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, c);

            node.Body.Accept(this, c);
        });
    }

    public void VisitNewArray(NewArrayExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);
    }

    public void VisitNewObject(NewObjectExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitNull(NullExpressionNode node, SymbolFinderContext context)
        => node.SymbolTable = context.SymbolTable;

    public void VisitReturn(ReturnStatementNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Expression?.Accept(this, context);
    }

    public void VisitParameter(ParameterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' parameter is already defined.");

        node.Type.Accept(this, context);
    }

    public void VisitProperty(PropertyDeclarationNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void VisitGetter(PropertyGetterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
            if (!c.SymbolTable.TryAddId(fieldSymbol))
                throw new SemanticAnalysisException();

            node.Body?.Accept(this, c);
        });
    }

    public void VisitSetter(PropertySetterNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        context.Scoped(c =>
        {
            c.DisableNextScope();

            var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
            if (!c.SymbolTable.TryAddId(fieldSymbol))
                throw new SemanticAnalysisException();

            var valueSymbol = new IdSymbol(MemberAccessExpressionNode.Value, node.Parent);
            if (!c.SymbolTable.TryAddId(valueSymbol))
                throw new SemanticAnalysisException();

            node.Body?.Accept(this, c);
        });
    }

    public void VisitTree(SyntaxTree node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var function in node.Declarations)
            function.Accept(this, context);
    }

    public void VisitTuple(TupleExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var expression in node.Expressions)
            expression.Accept(this, context);
    }

    public void VisitTupleType(TupleTypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        foreach (var type in node.Types)
            type.Accept(this, context);

        var symbol = TypeSymbol.Tuple(node);
        context.SymbolTable.TryAddType(symbol);
    }

    public void VisitTypeAlias(TypeAliasDeclarationNode node, SymbolFinderContext context)
    {
        if (!context.SymbolTable.TryAddType(TypeSymbol.Alias(node)))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var genericArgument in node.GenericArguments)
                genericArgument.Accept(this, c);

            node.Type.Accept(this, c);
        });
    }

    public void VisitType(TypeDeclarationNode node, SymbolFinderContext context)
    {
        var symbol = node.GenericArguments.Count > 0
            ? TypeSymbol.OpenGenericType(node)
            : TypeSymbol.Type(node);
        if (!context.SymbolTable.TryAddType(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        context.Scoped(c =>
        {
            node.SymbolTable = c.SymbolTable;

            foreach (var genericArgument in node.GenericArguments)
                genericArgument.Accept(this, c);

            foreach (var @interface in node.Interfaces)
                @interface.Accept(this, c);

            foreach (var property in node.Properties)
                property.Accept(this, c);

            foreach (var constructor in node.Constructors)
                constructor.Accept(this, c);

            foreach (var method in node.Methods)
                method.Accept(this, c);
        });
    }

    public void VisitTypeNode(TypeNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;
    }

    public void VisitUnaryExpression(UnaryExpressionNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Operand.Accept(this, context);
    }

    public void VisitVariable(VariableDeclarationStatementNode node, SymbolFinderContext context)
    {
        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.SymbolTable.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' variable is already defined.");

        node.SymbolTable = context.SymbolTable;
    }

    public void VisitWhile(WhileNode node, SymbolFinderContext context)
    {
        node.SymbolTable = context.SymbolTable;

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }
}