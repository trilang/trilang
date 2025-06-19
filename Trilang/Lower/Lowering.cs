using Trilang.Parsing.Ast;

namespace Trilang.Lower;

public class Lowering
{
    public void Lower(SyntaxTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        tree.Accept(new GenerateGettersAndSetters());
        tree.Accept(new AddThisInLocalMemberAccess());
        tree.Accept(new AddThisAsParameterToInstanceMethods());
        tree.Accept(new AddValueParameterToSetters());
    }
}