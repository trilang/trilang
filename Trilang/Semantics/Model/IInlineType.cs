using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public interface IInlineType : ISemanticNode
{
    string Name { get; }

    ITypeMetadata? Metadata { get; set; }

    IInlineType Clone();
}