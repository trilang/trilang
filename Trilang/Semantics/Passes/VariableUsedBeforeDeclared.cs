using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class VariableUsedBeforeDeclared : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;

    public VariableUsedBeforeDeclared(
        ISet<string> directives,
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap)
    {
        this.directives = directives;
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var visitor = new VariableUsedBeforeDeclaredVisitor(directives, diagnostics, symbolTableMap);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(VariableUsedBeforeDeclared);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];

    private sealed class VariableUsedBeforeDeclaredVisitor : Visitor
    {
        private readonly List<HashSet<string>> scopes;
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly SymbolTableMap symbolTableMap;

        public VariableUsedBeforeDeclaredVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            SymbolTableMap symbolTableMap)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            this.symbolTableMap = symbolTableMap;
            scopes = [];
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
    }
}