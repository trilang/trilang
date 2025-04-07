using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

// TODO: change result
public class VariableUsedBeforeDeclared : Visitor<VisitorContext<object>, object>
{
    private readonly Stack<HashSet<string>> scopes;

    public VariableUsedBeforeDeclared()
        => scopes = [];

    protected override void VisitEnter(BlockStatementNode node, VisitorContext<object> context)
        => scopes.Push([]);

    protected override void VisitExit(BlockStatementNode node, VisitorContext<object> context)
        => scopes.Pop();

    protected override void VisitEnter(VariableDeclarationStatementNode node, VisitorContext<object> context)
    {
        if (scopes.TryPeek(out var scope))
            scope.Add(node.Name);
    }

    protected override void VisitEnter(MemberAccessExpressionNode node, VisitorContext<object> context)
    {
        var symbol = node.SymbolTable?.GetVariable(node.Name) ??
                     throw new TypeCheckerException();

        if (symbol.Node is ParameterNode)
            return;

        if (!scopes.TryPeek(out var scope) || !scope.Contains(node.Name))
            throw new TypeCheckerException($"The '{node.Name}' variable used before declaration.");
    }
}