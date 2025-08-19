using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics.MetadataGenerators;
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

        var symbolFinder = new SymbolFinder(options);
        tree.Accept(symbolFinder, rootSymbolTable);

        var metadataGenerator = new MetadataGenerator(symbolFinder.Map);
        metadataGenerator.Generate(rootSymbolTable);

        // TODO: reorder
        tree.Accept(new ThisOutsideOfClass());
        tree.Accept(new ThisInStaticMethods());
        tree.Accept(new BreakContinueWithinLoop());
        tree.Accept(new VariableUsedBeforeDeclared(symbolFinder.Map));
        tree.Accept(new TypeChecker(options.Directives, symbolFinder.Map));
        tree.Accept(new NotImplementedInterface());
        tree.Accept(new CheckAccessModifiers());
        tree.Accept(new RecursiveTypeAlias());
        tree.Accept(new CheckStaticAndInstanceMembersAccess());
        tree.Accept(new RestrictFieldAccess());

        return new SemanticAnalysisResult(symbolFinder.Map, typeMetadataProvider);
    }
}