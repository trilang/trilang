using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

// TODO: split into two steps
internal class RecursiveTypeAlias : Visitor, ISemanticPass
{
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public RecursiveTypeAlias()
        => visitedTypes = [];

    public void Analyze(SemanticTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitTypeAliasEnter(TypeAliasDeclaration node)
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

    public string Name => nameof(RecursiveTypeAlias);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}