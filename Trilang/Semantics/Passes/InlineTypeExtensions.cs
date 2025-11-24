using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal static class InlineTypeExtensions
{
    internal static ITypeMetadata PopulateMetadata(
        this IInlineType node,
        IMetadataProvider metadataProvider,
        SemanticDiagnosticReporter diagnostics)
    {
        var metadata = metadataProvider.GetType(node.Name);
        if (metadata is null)
        {
            metadata = TypeMetadata.Invalid(node.Name);
            diagnostics.UnknownType(node);
        }

        node.Metadata = metadata;

        return metadata;
    }
}