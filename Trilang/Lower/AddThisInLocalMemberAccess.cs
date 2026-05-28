using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;

namespace Trilang.Lower;

internal class AddThisInLocalMemberAccess : Visitor
{
    private readonly DiagnosticCollection diagnostics;
    private readonly BuiltInTypes builtInTypes;
    private readonly MetadataProviderMap metadataProviderMap;

    public AddThisInLocalMemberAccess(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        BuiltInTypes builtInTypes,
        MetadataProviderMap metadataProviderMap) : base(directives)
    {
        this.diagnostics = diagnostics;
        this.builtInTypes = builtInTypes;
        this.metadataProviderMap = metadataProviderMap;
    }

    public override void VisitMemberAccess(MemberAccessExpression node)
    {
        base.VisitMemberAccess(node);

        if (node.Member is not null)
            return;

        if (node.IsThis || node.IsField || node.IsValue)
            return;

        if (node.Reference is not PropertyMetadata and not MethodMetadata)
            return;

        var parent = node.FindInParent<TypeDeclaration>()!;
        var metadataProvider = metadataProviderMap.Get(node);
        var metadataFactory = new MetadataFactory(builtInTypes, diagnostics.ForSemantic(), metadataProvider);
        var pointer = metadataFactory.CreatePointer(null, parent.Metadata!);

        node.Member = new MemberAccessExpression(null, MemberAccessExpression.This)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
            AccessKind = MemberAccessKind.Read,
        };
    }
}