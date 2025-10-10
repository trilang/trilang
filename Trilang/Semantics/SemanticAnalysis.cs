using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;
using Trilang.Semantics.Passes.ControlFlow;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    private readonly ITypeMetadataProvider typeMetadataProvider;

    // TODO: reflection?
    private readonly ISemanticPass[] semanticPasses =
    [
        new BreakContinueWithinLoop(),
        new CheckAccessModifiers(),
        new CheckStaticAndInstanceMembersAccess(),
        new ControlFlowAnalyzer(),
        new MemberAccessKindAnalyser(),
        new MetadataGenerator(),
        new MissingReturnStatement(),
        new NotImplementedInterface(),
        new CyclicTypeAlias(),
        new RestrictFieldAccess(),
        new SymbolFinder(),
        new ThisInStaticMethods(),
        new ThisOutsideOfClass(),
        new TypeChecker(),
        new VariableUsedBeforeDeclared()
    ];

    public SemanticAnalysis()
        : this(new RootTypeMetadataProvider())
    {
    }

    public SemanticAnalysis(ITypeMetadataProvider typeMetadataProvider)
        => this.typeMetadataProvider = typeMetadataProvider;

    public SemanticAnalysisResult Analyze(Parsing.Ast.SyntaxTree tree, SemanticAnalysisOptions options)
    {
        var semanticTree = (SemanticTree)tree.Transform(new ConvertToSemanticTree());

        var rootSymbolTable = new RootSymbolTable(typeMetadataProvider);
        var context = new SemanticPassContext(
            [.. options.Directives],
            options.Diagnostics,
            rootSymbolTable);

        foreach (var pass in GetSortedPasses())
            pass.Analyze(semanticTree, context);

        return new SemanticAnalysisResult(
            semanticTree,
            context.SymbolTableMap!,
            typeMetadataProvider,
            context.ControlFlowGraphs!);
    }

    private List<ISemanticPass> GetSortedPasses()
    {
        var passMap = semanticPasses.ToDictionary(p => p.Name, p => p);
        var inDegree = new Dictionary<string, int>();
        var graph = new Dictionary<string, List<string>>();

        foreach (var pass in semanticPasses)
        {
            var passName = pass.Name;

            inDegree[passName] = 0;
            graph[passName] = [];
        }

        foreach (var pass in semanticPasses)
        {
            var passName = pass.Name;

            foreach (var dependency in pass.DependsOn)
            {
                if (graph.TryGetValue(dependency, out var value))
                {
                    value.Add(passName);
                    inDegree[passName]++;
                }
            }
        }

        var queue = new Queue<string>();
        var result = new List<ISemanticPass>();

        foreach (var (key, value) in inDegree)
            if (value == 0)
                queue.Enqueue(key);

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            result.Add(passMap[current]);

            foreach (var neighbor in graph[current])
            {
                inDegree[neighbor]--;
                if (inDegree[neighbor] == 0)
                    queue.Enqueue(neighbor);
            }
        }

        Debug.Assert(result.Count == semanticPasses.Length, "Circular dependency detected in semantic passes.");

        return result;
    }
}