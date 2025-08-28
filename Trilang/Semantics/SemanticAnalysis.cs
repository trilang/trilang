using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics.Passes;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    private readonly ITypeMetadataProvider typeMetadataProvider;

    public SemanticAnalysis()
        : this(new RootTypeMetadataProvider())
    {
    }

    public SemanticAnalysis(ITypeMetadataProvider typeMetadataProvider)
        => this.typeMetadataProvider = typeMetadataProvider;

    public SemanticAnalysisResult Analyze(SyntaxTree tree, SemanticAnalysisOptions options)
    {
        var rootSymbolTable = new RootSymbolTable(typeMetadataProvider);
        var context = new SemanticPassContext([..options.Directives], rootSymbolTable);

        var passes = GetPasses();
        foreach (var pass in passes)
            pass.Analyze(tree, context);

        return new SemanticAnalysisResult(context.SymbolTableMap!, typeMetadataProvider);
    }

    private List<ISemanticPass> GetPasses()
    {
        // TODO: reflection?
        var passes = new ISemanticPass[]
        {
            new BreakContinueWithinLoop(),
            new CheckAccessModifiers(),
            new CheckStaticAndInstanceMembersAccess(),
            new MemberAccessKindAnalyser(),
            new MetadataGenerator(),
            new NotImplementedInterface(),
            new RecursiveTypeAlias(),
            new RestrictFieldAccess(),
            new SymbolFinder(),
            new ThisInStaticMethods(),
            new ThisOutsideOfClass(),
            new TypeChecker(),
            new VariableUsedBeforeDeclared(),
        };

        var passMap = passes.ToDictionary(p => p.Name, p => p);
        var inDegree = new Dictionary<string, int>();
        var graph = new Dictionary<string, List<string>>();

        foreach (var pass in passes)
        {
            var passName = pass.Name;

            inDegree[passName] = 0;
            graph[passName] = [];
        }

        foreach (var pass in passes)
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

        foreach (var kvp in inDegree)
            if (kvp.Value == 0)
                queue.Enqueue(kvp.Key);

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

        Debug.Assert(result.Count == passes.Length, "Circular dependency detected in semantic passes.");

        return result;
    }
}