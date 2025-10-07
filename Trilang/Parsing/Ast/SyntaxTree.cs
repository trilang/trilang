using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class SyntaxTree : ISyntaxNode
{
    private readonly List<IDeclarationNode> declarations;

    public SyntaxTree(SourceFile sourceFile, IReadOnlyList<IDeclarationNode> declarations)
    {
        SourceFile = sourceFile;
        SourceSpan = declarations switch
        {
            [] => default,
            [var single] => single.SourceSpan,
            [var first, .., var last] => first.SourceSpan.Combine(last.SourceSpan),
        };

        this.declarations = [.. declarations];
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitTree(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTree(this);

    public SourceFile SourceFile { get; }

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<IDeclarationNode> Declarations
        => declarations;
}