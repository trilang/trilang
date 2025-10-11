using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;
using Trilang.Symbols;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics.Passes;

internal class SymbolFinder : IVisitor<ISymbolTable>, ISemanticPass
{
    private readonly SymbolTableMap map;
    private HashSet<string> directives = null!;
    private SemanticDiagnosticReporter diagnostics = null!;

    public SymbolFinder()
        => map = new SymbolTableMap();

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        directives = context.Directives;
        diagnostics = context.Diagnostics;

        tree.Accept(this, context.RootSymbolTable);

        context.SymbolTableMap = map;
    }

    public void VisitArrayAccess(ArrayAccessExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);
        node.Index.Accept(this, context);
    }

    public void VisitArrayType(ArrayType node, ISymbolTable context)
    {
        map.Add(node, context);

        context.TryAddType(TypeSymbol.Array(node));

        node.ElementType.Accept(this, context);
    }

    public void VisitBinaryExpression(BinaryExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Left.Accept(this, context);
        node.Right.Accept(this, context);
    }

    private void VisitBlockWithoutScope(BlockStatement node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var statement in node.Statements)
            statement.Accept(this, context);
    }

    public void VisitBlock(BlockStatement node, ISymbolTable context)
        => VisitBlockWithoutScope(node, context.CreateChild());

    public void VisitBreak(Break node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitCall(CallExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitCast(CastExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);
    }

    public void VisitConstructor(ConstructorDeclaration node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        child.TryAddId(new IdSymbol(MemberAccessExpression.This, node.Parent));

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitContinue(Continue node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitDiscriminatedUnion(DiscriminatedUnion node, ISymbolTable context)
    {
        var symbol = TypeSymbol.DiscriminatedUnion(node);
        context.TryAddType(symbol);

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var type in node.Types)
            type.Accept(this, child);
    }

    public void VisitExpressionBlock(ExpressionBlock node, ISymbolTable context)
        => Debug.Fail("Expression blocks are the compiler's internal feature and are not directly supported in the programming language.");

    public void VisitExpressionStatement(ExpressionStatement node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
    }

    public void VisitFakeDeclaration(FakeDeclaration node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitFakeExpression(FakeExpression node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitFakeStatement(FakeStatement node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitFakeType(FakeType node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitFunction(FunctionDeclaration node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.FunctionAlreadyDefined(node);

        node.ReturnType.Accept(this, context);

        var child = context.CreateChild();
        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitFunctionType(FunctionType node, ISymbolTable context)
    {
        var symbol = TypeSymbol.FunctionType(node);
        context.TryAddType(symbol);

        map.Add(node, context);

        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);
    }

    public void VisitGenericType(GenericType node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);

        var symbol = TypeSymbol.GenericType(node);
        context.TryAddType(symbol);
    }

    public void VisitGoTo(GoTo node, ISymbolTable context)
    {
    }

    public void VisitIfDirective(IfDirective node, ISymbolTable context)
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

    public void VisitIf(IfStatement node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);
    }

    public void VisitInterface(Interface node, ISymbolTable context)
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

    public void VisitInterfaceProperty(InterfaceProperty node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.InterfacePropertyAlreadyDefined(node);
    }

    public void VisitInterfaceMethod(InterfaceMethod node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.InterfaceMethodAlreadyDefined(node);

        var child = context.CreateChild();
        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, child);

        node.ReturnType.Accept(this, child);
    }

    public void VisitIsExpression(IsExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);
    }

    public void VisitLabel(Label node, ISymbolTable context)
    {
    }

    public void VisitLiteral(LiteralExpression node, ISymbolTable context)
    {
        map.Add(node, context);
    }

    public void VisitMemberAccess(MemberAccessExpression node, ISymbolTable context)
    {
        node.Member?.Accept(this, context);

        map.Add(node, context);
    }

    public void VisitMethod(MethodDeclaration node, ISymbolTable context)
    {
        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.MethodAlreadyDefined(node);

        map.Add(node, context);

        node.ReturnType.Accept(this, context);

        var child = context.CreateChild();
        child.TryAddId(new IdSymbol(MemberAccessExpression.This, node.Parent));

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, child);

        VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitNewArray(NewArrayExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);
        node.Size.Accept(this, context);
    }

    public void VisitNewObject(NewObjectExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);
    }

    public void VisitNull(NullExpression node, ISymbolTable context)
        => map.Add(node, context);

    public void VisitReturn(ReturnStatement node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Expression?.Accept(this, context);
    }

    public void VisitParameter(Parameter node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.ParameterAlreadyDefined(node);

        node.Type.Accept(this, context);
    }

    public void VisitProperty(PropertyDeclaration node, ISymbolTable context)
    {
        map.Add(node, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.PropertyAlreadyDefined(node);

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void VisitGetter(PropertyGetter node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        var fieldSymbol = new IdSymbol(MemberAccessExpression.Field, node.Parent);
        if (!child.TryAddId(fieldSymbol))
            diagnostics.ParameterAlreadyDefined(node, MemberAccessExpression.Field);

        if (node.Body is not null)
            VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitSetter(PropertySetter node, ISymbolTable context)
    {
        map.Add(node, context);

        var child = context.CreateChild();
        var fieldSymbol = new IdSymbol(MemberAccessExpression.Field, node.Parent);
        if (!child.TryAddId(fieldSymbol))
            diagnostics.ParameterAlreadyDefined(node, MemberAccessExpression.Field);

        var valueSymbol = new IdSymbol(MemberAccessExpression.Value, node.Parent);
        if (!child.TryAddId(valueSymbol))
            diagnostics.ParameterAlreadyDefined(node, MemberAccessExpression.Value);

        if (node.Body is not null)
            VisitBlockWithoutScope(node.Body, child);
    }

    public void VisitTree(SemanticTree node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var function in node.Declarations)
            function.Accept(this, context);
    }

    public void VisitTuple(TupleExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var expression in node.Expressions)
            expression.Accept(this, context);
    }

    public void VisitTupleType(TupleType node, ISymbolTable context)
    {
        map.Add(node, context);

        foreach (var type in node.Types)
            type.Accept(this, context);

        var symbol = TypeSymbol.Tuple(node);
        context.TryAddType(symbol);
    }

    public void VisitTypeAlias(TypeAliasDeclaration node, ISymbolTable context)
    {
        if (!context.TryAddType(TypeSymbol.Alias(node)))
            diagnostics.TypeAlreadyDefined(node);

        var child = context.CreateChild();
        map.Add(node, child);

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this, child);

        node.Type.Accept(this, child);
    }

    public void VisitType(TypeDeclaration node, ISymbolTable context)
    {
        var symbol = node.GenericArguments.Count > 0
            ? TypeSymbol.OpenGenericType(node)
            : TypeSymbol.Type(node);
        if (!context.TryAddType(symbol))
            diagnostics.TypeAlreadyDefined(node);

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

    public void VisitTypeNode(Type node, ISymbolTable context)
    {
        map.Add(node, context);
    }

    public void VisitUnaryExpression(UnaryExpression node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Operand.Accept(this, context);
    }

    public void VisitVariable(VariableDeclaration node, ISymbolTable context)
    {
        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        var symbol = new IdSymbol(node);
        if (!context.TryAddId(symbol))
            diagnostics.VariableAlreadyDefined(node);

        map.Add(node, context);
    }

    public void VisitWhile(While node, ISymbolTable context)
    {
        map.Add(node, context);

        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);
    }

    public string Name => nameof(SymbolFinder);

    public IEnumerable<string> DependsOn => [];
}