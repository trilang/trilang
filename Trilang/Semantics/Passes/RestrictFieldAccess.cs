using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class RestrictFieldAccess : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        if (node.IsFirstMember)
            return;

        if (node.Reference is FieldMetadata)
            throw new SemanticAnalysisException($"The '{node.Name}' field is not accessible.");
    }

    public string Name => nameof(RestrictFieldAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}