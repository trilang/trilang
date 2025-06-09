using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class VariableUsedBeforeDeclared : Visitor
{
    private readonly Stack<HashSet<string>> scopes;

    public VariableUsedBeforeDeclared()
        => scopes = [];

    protected override void VisitBlockEnter(BlockStatementNode node)
        => scopes.Push([]);

    protected override void VisitBlockExit(BlockStatementNode node)
        => scopes.Pop();

    protected override void VisitVariableEnter(VariableDeclarationStatementNode node)
    {
        if (scopes.TryPeek(out var scope))
            scope.Add(node.Name);
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        if (node.Member is not null || node.IsThis || node.IsField || node.IsValue)
            return;

        var symbol = node.SymbolTable?.GetId(node.Name);
        if (symbol is not null)
        {
            if (symbol.Node is ParameterNode or FunctionDeclarationNode)
                return;

            if (!scopes.TryPeek(out var scope) || !scope.Contains(node.Name))
                throw new SemanticAnalysisException($"The '{node.Name}' variable used before declaration.");
        }
        else
        {
            // access static member
            var typeProvider = node.SymbolTable!.TypeProvider;
            var type = typeProvider.GetType(node.Name);
            if (type is null)
                throw new SemanticAnalysisException($"Unknown symbol: {node.Name}");
        }
    }
}