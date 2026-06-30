using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class AddThisInLocalMemberAccess : Visitor
{
    private readonly DiagnosticCollection diagnostics;
    private readonly BuiltInTypes builtInTypes;

    public AddThisInLocalMemberAccess(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        BuiltInTypes builtInTypes) : base(directives)
    {
        this.diagnostics = diagnostics;
        this.builtInTypes = builtInTypes;
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
        var metadataProvider = node.MetadataProvider!;
        var metadataFactory = new MetadataFactory(builtInTypes, diagnostics.ForSemantic(), metadataProvider);
        var pointer = metadataFactory.CreatePointer(null, parent.Metadata!);

        node.Member = new MemberAccessExpression(null, MemberAccessExpression.This)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
            AccessKind = MemberAccessKind.Read,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
        };
    }
}