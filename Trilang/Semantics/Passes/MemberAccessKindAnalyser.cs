using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class MemberAccessKindAnalyser : Visitor, ISemanticPass
{
    public MemberAccessKindAnalyser(ISet<string> directives) : base(directives)
    {
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    private static MemberAccessKind FindParentAssignment(MemberAccessExpression node)
    {
        var parent = node.Parent;
        while (parent is not null)
        {
            if (parent is BinaryExpression { Kind: BinaryExpressionKind.Assignment } assignment)
            {
                if (ReferenceEquals(assignment.Left, node))
                    return MemberAccessKind.Write;

                return MemberAccessKind.Read;
            }

            if (parent is BinaryExpression { IsCompoundAssignment: true } compound)
            {
                if (ReferenceEquals(compound.Left, node))
                    return MemberAccessKind.ReadWrite;

                return MemberAccessKind.Read;
            }

            parent = parent.Parent;
        }

        return MemberAccessKind.Read;
    }

    public override void VisitMemberAccess(MemberAccessExpression node)
    {
        node.AccessKind = FindParentAssignment(node);

        base.VisitMemberAccess(node);
    }

    public string Name => nameof(MemberAccessKindAnalyser);

    public IEnumerable<string> DependsOn => [];
}