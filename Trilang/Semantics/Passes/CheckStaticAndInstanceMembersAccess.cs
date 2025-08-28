using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class CheckStaticAndInstanceMembersAccess : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        if (node.Member is not MemberAccessExpressionNode parent)
            return;

        if (node.Reference is null)
            return;

        var parentRef = parent.Reference;
        if (parentRef is null)
            return;

        if (parentRef is ITypeMetadata type)
        {
            if (type.UnpackAlias() is TypeMetadata)
            {
                if (node.Reference is MethodMetadata { IsStatic: false } method)
                    throw new SemanticAnalysisException($"The instance method '{method.Name}' cannot be called on a static one.");
            }
            else
            {
                throw new SemanticAnalysisException($"'{parentRef}' can't be used to call static members.");
            }
        }
        else
        {
            if (node.Reference is MethodMetadata { IsStatic: true } method)
                throw new SemanticAnalysisException($"The static method '{method.Name}' cannot be called on an instance one.");
        }
    }

    public string Name => nameof(CheckStaticAndInstanceMembersAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}