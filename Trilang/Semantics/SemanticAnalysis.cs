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

    public void Analyze(SyntaxTree tree, SemanticAnalysisOptions options)
    {
        var rootSymbolTable = new RootSymbolTable(typeMetadataProvider);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext(rootSymbolTable, options));
        tree.Accept(new ThisOutsideOfClass());
        tree.Accept(new ThisInStaticMethods());
        tree.Accept(new BreakContinueWithinLoop());
        tree.Accept(new MetadataGenerator());
        tree.Accept(new VariableUsedBeforeDeclared());
        tree.Accept(new TypeChecker(), new TypeCheckerContext(options.Directives));
        tree.Accept(new NotImplementedInterface());
        tree.Accept(new CheckAccessModifiers());
        tree.Accept(new RecursiveTypeAlias());
        tree.Accept(new CheckStaticAndInstanceMembersAccess());
    }
}