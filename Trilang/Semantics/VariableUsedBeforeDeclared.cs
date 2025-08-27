using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class VariableUsedBeforeDeclared : Visitor
{
    private readonly SymbolTableMap symbolFinderMap;
    private readonly List<HashSet<string>> scopes;

    public VariableUsedBeforeDeclared(SymbolTableMap symbolFinderMap)
    {
        this.symbolFinderMap = symbolFinderMap;
        scopes = [];
    }

    protected override void VisitBlockEnter(BlockStatementNode node)
        => scopes.Add([]);

    protected override void VisitBlockExit(BlockStatementNode node)
        => scopes.RemoveAt(scopes.Count - 1);

    protected override void VisitVariableEnter(VariableDeclarationStatementNode node)
        => scopes[^1].Add(node.Name);

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsFirstMember || node.IsThis || node.IsField || node.IsValue)
            return;

        var symbolTable = symbolFinderMap.Get(node);
        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null)
        {
            if (symbol.Node is ParameterNode
                or FunctionDeclarationNode
                or PropertyDeclarationNode
                or MethodDeclarationNode)
                return;

            for (var i = scopes.Count - 1; i >= 0; i--)
                if (scopes[i].Contains(node.Name))
                    return;

            throw new SemanticAnalysisException($"The '{node.Name}' variable used before declaration.");
        }

        // access static member
        var typeProvider = symbolTable.TypeProvider;
        var type = typeProvider.GetType(node.Name);
        if (type is null)
            throw new SemanticAnalysisException($"Unknown symbol: {node.Name}");
    }
}