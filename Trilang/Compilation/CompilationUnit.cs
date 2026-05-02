using Trilang.Parsing.Ast;
using Trilang.Semantics.Model;

namespace Trilang.Compilation;

public record CompilationUnit(SourceFile File)
{
    public override string ToString()
        => File.ToString();

    public SyntaxTree? SyntaxTree { get; set; }

    public SemanticTree? SemanticTree { get; set; }
}