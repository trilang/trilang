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
        if (node.Member is not null || node.IsThis)
            return;

        var symbol = node.SymbolTable?.GetId(node.Name) ??
                     throw new SemanticAnalysisException($"Unknown symbol: {node.Name}");

        if (symbol.Node is ParameterNode or FunctionDeclarationNode)
            return;

        if (!scopes.TryPeek(out var scope) || !scope.Contains(node.Name))
            throw new SemanticAnalysisException($"The '{node.Name}' variable used before declaration.");
    }
}