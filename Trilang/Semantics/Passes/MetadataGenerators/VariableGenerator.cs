using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class VariableGenerator : Visitor
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly MetadataProviderMap metadataProviderMap;

    private SourceFile file;

    public VariableGenerator(
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap,
        MetadataProviderMap metadataProviderMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        this.metadataProviderMap = metadataProviderMap;
    }

    protected override void VisitTreeEnter(SemanticTree node)
        => file = node.SourceFile;

    protected override void VisitVariableEnter(VariableDeclaration node)
    {
        var symbolTable = symbolTableMap.Get(node);
        var typeProvider = metadataProviderMap.Get(node);

        var type = node.Type.PopulateMetadata(typeProvider, diagnostics);
        var metadata = new VariableMetadata(
            new SourceLocation(file, node.SourceSpan.GetValueOrDefault()),
            node.Name,
            type);

        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null && symbol.Nodes[0] != node)
        {
            metadata.MarkAsInvalid();
            diagnostics.VariableAlreadyDefined(node);
        }

        node.Metadata = metadata;
    }
}