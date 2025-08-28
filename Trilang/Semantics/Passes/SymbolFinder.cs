using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

internal class SymbolFinder : IVisitor<ISymbolTable>, ISemanticPass
{
    private readonly SymbolTableMap map;
    private HashSet<string> directives = null!;

    public SymbolFinder()
        => map = new SymbolTableMap();

    public void Analyze(SyntaxTree tree, SemanticPassContext context)
    {
        directives = context.Directives;

        tree.Accept(this, context.RootSymbolTable);

        context.SymbolTableMap = map;
    }

    public void VisitArrayAccess(ArrayAccessExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

    public void VisitArrayType(ArrayTypeNode node, ISymbolTable context)
    {
        map.Add(node, context);

        context.TryAddType(TypeSymbol.Array(node));

        node.ElementType.Accept(this, context);
    }

    public void VisitBinaryExpression(BinaryExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    private void VisitBlockWithoutScope(BlockStatementNode node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);
    }

    public void VisitBlock(BlockStatementNode node, ISymbolTable context)
        => VisitBlockWithoutScope(node, context.CreateChild());

    public void VisitBreak(BreakNode node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitCall(CallExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitCast(CastExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);
    }

    public void VisitConstructor(ConstructorDeclarationNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        child.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitContinue(ContinueNode node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node, ISymbolTable context)
    {
        var symbol = TypeSymbol.DiscriminatedUnion(node);
        context.TryAddType(symbol);

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var type in node.Types)
            type.Accept(this, child);
    }

    public void VisitExpressionBlock(ExpressionBlockNode node, ISymbolTable context)
        => throw new SemanticAnalysisException("Expression blocks are not supported");

    public void VisitExpressionStatement(ExpressionStatementNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
    }

    public void VisitFunction(FunctionDeclarationNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' function is already defined.");

        node.ReturnType.Accept(this, context);

        var child = context.CreateChild();
        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitFunctionType(FunctionTypeNode node, ISymbolTable context)
    {
        var symbol = TypeSymbol.FunctionType(node);
        context.TryAddType(symbol);

        map.Add(node, context);

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void VisitGenericType(GenericTypeNode node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);

        var symbol = TypeSymbol.GenericType(node);
        context.TryAddType(symbol);
    }

    public void VisitGoTo(GoToNode node, ISymbolTable context)
    {
    }

    public void VisitIfDirective(IfDirectiveNode node, ISymbolTable context)
    {
        if (directives.Contains(node.DirectiveName))
        {
            foreach (var then in node.Then)
                then.Accept(this, context);
        }
        else
        {
            foreach (var @else in node.Else)
                @else.Accept(this, context);
        }
    }

    public void VisitIf(IfStatementNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void VisitInterface(InterfaceNode node, ISymbolTable context)
    {
        var symbol = TypeSymbol.Interface(node);
        context.TryAddType(symbol);

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var property in node.Properties)
            property.Accept(this, child);

        foreach (var method in node.Methods)
            method.Accept(this, child);
    }

    public void VisitInterfaceProperty(InterfacePropertyNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");
    }

    public void VisitInterfaceMethod(InterfaceMethodNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        var child = context.CreateChild();
        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, child);

        node.ReturnType.Accept(this, child);
    }

    public void VisitAsExpression(IsExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);
    }

    public void VisitLabel(LabelNode node, ISymbolTable context)
    {
    }

    public void VisitLiteral(LiteralExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);
    }

    public void VisitMemberAccess(MemberAccessExpressionNode node, ISymbolTable context)
    {
        node.Member?.Accept(this, context);

        map.Add(node, context);
    }

    public void VisitMethod(MethodDeclarationNode node, ISymbolTable context)
    {
        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' method is already defined.");

        map.Add(node, context);

        node.ReturnType.Accept(this, context);

        var child = context.CreateChild();
        child.TryAddId(new IdSymbol(MemberAccessExpressionNode.This, node.Parent));

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitNewArray(NewArrayExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);
    }

    public void VisitNewObject(NewObjectExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitNull(NullExpressionNode node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitReturn(ReturnStatementNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression?.Accept(this, context);
    }

    public void VisitParameter(ParameterNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' parameter is already defined.");

        node.Type.Accept(this, context);
    }

    public void VisitProperty(PropertyDeclarationNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' property is already defined.");

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void VisitGetter(PropertyGetterNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
        if (!child.TryAddId(fieldSymbol))
            throw new SemanticAnalysisException();

        if (node.Body is not null)
            VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitSetter(PropertySetterNode node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        var fieldSymbol = new IdSymbol(MemberAccessExpressionNode.Field, node.Parent);
        if (!child.TryAddId(fieldSymbol))
            throw new SemanticAnalysisException();

        var valueSymbol = new IdSymbol(MemberAccessExpressionNode.Value, node.Parent);
        if (!child.TryAddId(valueSymbol))
            throw new SemanticAnalysisException();

        if (node.Body is not null)
            VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitTree(SyntaxTree node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var function in node.Declarations)
            function.Accept(this, context);
    }

    public void VisitTuple(TupleExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var expression in node.Expressions)
            expression.Accept(this, context);
    }

    public void VisitTupleType(TupleTypeNode node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        var symbol = TypeSymbol.Tuple(node);
        context.TryAddType(symbol);
    }

    public void VisitTypeAlias(TypeAliasDeclarationNode node, ISymbolTable context)
    {
        if (!context.TryAddType(TypeSymbol.Alias(node)))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this, child);

        node.Type.Accept(this, child);
    }

    public void VisitType(TypeDeclarationNode node, ISymbolTable context)
    {
        var symbol = node.GenericArguments.Count > 0
            ? TypeSymbol.OpenGenericType(node)
            : TypeSymbol.Type(node);
        if (!context.TryAddType(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' type is already defined.");

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this, child);

        foreach (var @interface in node.Interfaces)
            @interface.Accept(this, child);

        foreach (var property in node.Properties)
            property.Accept(this, child);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this, child);

        foreach (var method in node.Methods)
            method.Accept(this, child);
    }

    public void VisitTypeNode(TypeNode node, ISymbolTable context)
    {
        map.Add(node, context);
    }

    public void VisitUnaryExpression(UnaryExpressionNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Operand.Accept(this, context);
    }

    public void VisitVariable(VariableDeclarationStatementNode node, ISymbolTable context)
    {
        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            throw new SemanticAnalysisException($"The '{node.Name}' variable is already defined.");

        map.Add(node, context);
    }

    public void VisitWhile(WhileNode node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }

    public string Name => nameof(SymbolFinder);

    public IEnumerable<string> DependsOn => [];
}