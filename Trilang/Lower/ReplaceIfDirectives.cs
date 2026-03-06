using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplaceIfDirectives : Visitor
{
    public ReplaceIfDirectives(ISet<string> directives) : base(directives)
    {
    }

    public override void VisitTree(SemanticTree node)
    {
        for (var i = 0; i < node.Declarations.Count; i++)
        {
            var declaration = node.Declarations[i];
            if (declaration is not IfDirective ifDirective)
                continue;

            node.Remove(declaration);

            var statements = ifDirective.Else;
            if (directives.Contains(ifDirective.DirectiveName))
                statements = ifDirective.Then;

            foreach (var statement in statements)
                node.Insert(i, (IDeclaration)statement);

            i--;
        }

        base.VisitTree(node);
    }

    public override void VisitBlock(BlockStatement node)
    {
        for (var i = 0; i < node.Statements.Count; i++)
        {
            var statement = node.Statements[i];
            if (statement is not IfDirective ifDirective)
                continue;

            node.Remove(statement);

            var statements = ifDirective.Else;
            if (directives.Contains(ifDirective.DirectiveName))
                statements = ifDirective.Then;

            foreach (var nestedStatement in statements)
                node.Insert(i, (IStatement)nestedStatement);

            i--;
        }

        base.VisitBlock(node);
    }
}