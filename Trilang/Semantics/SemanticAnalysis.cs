using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    public void Analyze(SyntaxTree tree)
    {
        tree.Accept(new SymbolFinder(), new SymbolFinderContext());
        tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>());

        var typeMetadataProvider = new TypeMetadataProvider();
        tree.Accept(new GenerateMetadata(typeMetadataProvider));
        tree.Accept(new TypeChecker(typeMetadataProvider));
    }
}