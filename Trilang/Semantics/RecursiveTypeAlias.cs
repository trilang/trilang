using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class RecursiveTypeAlias : Visitor
{
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public RecursiveTypeAlias()
        => visitedTypes = [];

    protected override void VisitEnter(TypeAliasDeclarationNode node)
    {
        var metadata = node.Metadata;
        if (metadata is null)
            return;

        CheckCircularReference(metadata);
    }

    private void CheckCircularReference(ITypeMetadata metadata)
    {
        visitedTypes.Clear();

        while (true)
        {
            if (metadata is not TypeAliasMetadata alias)
                return;

            if (!visitedTypes.Add(alias))
                throw new SemanticAnalysisException($"The recursive type alias detected: '{metadata}'.");

            metadata = alias.Type!;
        }
    }
}