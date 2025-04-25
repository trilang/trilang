using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    public SemanticAnalysis()
        => TypeProvider = new TypeMetadataProvider();

    public void Analyze(SyntaxTree tree)
    {
        tree.Accept(new SymbolFinder(), new SymbolFinderContext());
        tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>());
        tree.Accept(new ThisOutsideOfClass());
        tree.Accept(new GenerateMetadata(TypeProvider));
        tree.Accept(new TypeChecker(TypeProvider));
    }

    public TypeMetadataProvider TypeProvider { get; }
}