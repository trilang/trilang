using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class VariableUsedBeforeDeclared : Visitor, ISemanticPass
{
    private readonly List<HashSet<string>> scopes;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;

    public VariableUsedBeforeDeclared(
        ISet<string> directives,
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap) : base(directives)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        scopes = [];
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    public override void VisitBlock(BlockStatement node)
    {
        scopes.Add([]);

        base.VisitBlock(node);

        scopes.RemoveAt(scopes.Count - 1);
    }

    public override void VisitVariable(VariableDeclaration node)
    {
        scopes[^1].Add(node.Name);

        base.VisitVariable(node);
    }

    public override void VisitMemberAccess(MemberAccessExpression node)
    {
        if (!node.IsFirstMember || node.IsThis || node.IsField || node.IsValue)
            return;

        var symbolTable = symbolTableMap.Get(node);
        var symbols = symbolTable.GetId(node.Name);
        if (symbols is [])
            return;

        if (!symbols.Any(x => x.Node is VariableDeclaration))
            return;

        for (var i = scopes.Count - 1; i >= 0; i--)
            if (scopes[i].Contains(node.Name))
                return;

        diagnostics.VariableUsedBeforeDeclaration(node);

        base.VisitMemberAccess(node);
    }

    public string Name => nameof(VariableUsedBeforeDeclared);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}