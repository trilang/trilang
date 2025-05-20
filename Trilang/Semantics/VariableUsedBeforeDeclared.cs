using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class VariableUsedBeforeDeclared : Visitor
{
    private readonly Stack<HashSet<string>> scopes;

    public VariableUsedBeforeDeclared()
        => scopes = [];

    protected override void VisitEnter(BlockStatementNode node)
        => scopes.Push([]);

    protected override void VisitExit(BlockStatementNode node)
        => scopes.Pop();

    protected override void VisitEnter(VariableDeclarationStatementNode node)
    {
        if (scopes.TryPeek(out var scope))
            scope.Add(node.Name);
    }

    protected override void VisitEnter(MemberAccessExpressionNode node)
    {
        if (node.Member is not null || node.IsThis || node.IsField || node.IsValue)
            return;

        var symbol = node.SymbolTable?.GetId(node.Name) ??
                     throw new SemanticAnalysisException($"Unknown symbol: {node.Name}");

        if (symbol.Node is ParameterNode or FunctionDeclarationNode)
            return;

        if (!scopes.TryPeek(out var scope) || !scope.Contains(node.Name))
            throw new SemanticAnalysisException($"The '{node.Name}' variable used before declaration.");
    }
}