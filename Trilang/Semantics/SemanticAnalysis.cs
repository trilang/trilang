using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    public SemanticAnalysisResult Analyze(
        IEnumerable<Parsing.Ast.SyntaxTree> syntaxTrees,
        SemanticAnalysisOptions options)
    {
        var converter = new ConvertToSemanticTree(options.BuiltInTypes);
        var semanticTrees = syntaxTrees
            .Select(x => x.Transform(converter))
            .Cast<SemanticTree>()
            .ToArray();

        var rootNamespace = NamespaceMetadata.CreateRoot(options.BuiltInTypes);
        var symbolTableMap = new SymbolTableMap();
        var metadataProviderMap = new MetadataProviderMap();
        var controlFlowGraphMap = new ControlFlowGraphMap();
        var semanticPasses = CreateSemanticPasses(
            options,
            symbolTableMap,
            metadataProviderMap,
            controlFlowGraphMap,
            rootNamespace,
            options.BuiltInTypes);

        foreach (var pass in GetSortedPasses(semanticPasses))
            pass.Analyze(semanticTrees);

        return new SemanticAnalysisResult(
            semanticTrees,
            symbolTableMap,
            rootNamespace,
            controlFlowGraphMap);
    }

    private ISemanticPass[] CreateSemanticPasses(
        SemanticAnalysisOptions options,
        SymbolTableMap symbolTableMap,
        MetadataProviderMap metadataProviderMap,
        ControlFlowGraphMap controlFlowGraphMap,
        NamespaceMetadata rootNamespace,
        BuiltInTypes builtInTypes)
        =>
        [
            new BreakContinueWithinLoop(options.Directives, options.Diagnostics),
            new CheckAccessModifiers(options.Directives, options.Diagnostics),
            new CheckStaticAndInstanceMembersAccess(options.Directives, options.Diagnostics),
            new ControlFlowAnalyzer(options.Directives, controlFlowGraphMap),
            new MemberAccessKindAnalyser(options.Directives),
            new MetadataGenerator(
                options.Directives,
                options.Diagnostics,
                symbolTableMap,
                metadataProviderMap,
                rootNamespace,
                builtInTypes),
            new MissingReturnStatement(options.Diagnostics, controlFlowGraphMap, builtInTypes),
            new NotImplementedInterface(options.Directives, options.Diagnostics),
            new CyclicAlias(options.Diagnostics, rootNamespace),
            new PrivateInterfaceProperties(options.Diagnostics, rootNamespace),
            new RestrictFieldAccess(options.Directives, options.Diagnostics),
            new SymbolFinder(options.Directives, symbolTableMap),
            new ThisInStaticMethods(options.Directives, options.Diagnostics),
            new ThisOutsideOfType(options.Directives, options.Diagnostics),
            new TypeChecker(
                options.Directives,
                options.Diagnostics,
                symbolTableMap,
                metadataProviderMap,
                options.BuiltInTypes),
            new UnresolvedMemberAccess(options.Directives, options.Diagnostics),
            new VariableUsedBeforeDeclared(options.Directives, options.Diagnostics, symbolTableMap),
        ];

    private List<ISemanticPass> GetSortedPasses(ISemanticPass[] semanticPasses)
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