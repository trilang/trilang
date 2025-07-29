using Trilang.Parsing.Ast;

namespace Trilang.Lower;

public class Lowering
{
    public void Lower(SyntaxTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        tree.Accept(new AddThisInLocalMemberAccess());
        tree.Accept(new GenerateGettersAndSetters());
        tree.Transform(new ReplaceCompoundAssignments());
        tree.Transform(new ReplacePropertyFieldAndValueWithGeneratedField());
        tree.Transform(new ReplaceGettersAndSettersWithMethodCalls());
        tree.Accept(new ReplaceWhileLoop());
        tree.Accept(new RewriteIfStatement());
    }
}