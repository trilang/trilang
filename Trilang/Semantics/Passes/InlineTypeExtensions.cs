using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal static class InlineTypeExtensions
{
    internal static ITypeMetadata PopulateMetadata(
        this IInlineType node,
        ITypeMetadataProvider typeProvider,
        SemanticDiagnosticReporter diagnostics)
    {
        var metadata = typeProvider.GetType(node.Name);
        if (metadata is null)
        {
            metadata = TypeMetadata.Invalid(node.Name);
            diagnostics.UnknownType(node);
        }

        node.Metadata = metadata;

        return metadata;
    }
}