using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class VariableUsedBeforeDeclared : Visitor, ISemanticPass
{
    private readonly List<HashSet<string>> scopes;
    private SymbolTableMap symbolFinderMap = null!;

    public VariableUsedBeforeDeclared()
        => scopes = [];

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        symbolFinderMap = context.SymbolTableMap!;

        tree.Accept(this);
    }

    protected override void VisitBlockEnter(BlockStatement node)
        => scopes.Add([]);

    protected override void VisitBlockExit(BlockStatement node)
        => scopes.RemoveAt(scopes.Count - 1);

    protected override void VisitVariableEnter(VariableDeclaration node)
        => scopes[^1].Add(node.Name);

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (!node.IsFirstMember || node.IsThis || node.IsField || node.IsValue)
            return;

        var symbolTable = symbolFinderMap.Get(node);
        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null)
        {
            if (symbol.Node is Parameter
                or FunctionDeclaration
                or PropertyDeclaration
                or MethodDeclaration)
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

    public string Name => nameof(VariableUsedBeforeDeclared);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}