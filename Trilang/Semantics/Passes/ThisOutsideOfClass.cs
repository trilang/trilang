using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class ThisOutsideOfClass : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsThis)
            return;

        var type = node.FindInParent<TypeDeclarationNode>();
        if (type is not null)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is only allowed inside a type.");
    }

    public string Name => nameof(ThisOutsideOfClass);

    public IEnumerable<string> DependsOn => [];
}