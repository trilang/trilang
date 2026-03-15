using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class MemberAccessKindAnalyser : ISemanticPass
{
    private readonly ISet<string> directives;

    public MemberAccessKindAnalyser(ISet<string> directives)
    {
        this.directives = directives;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var visitor = new MemberAccessKindAnalyserVisitor(directives);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(MemberAccessKindAnalyser);

    public IEnumerable<string> DependsOn => [];

    private sealed class MemberAccessKindAnalyserVisitor : Visitor
    {
        public MemberAccessKindAnalyserVisitor(ISet<string> directives)
            : base(directives)
        {
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
    }
}