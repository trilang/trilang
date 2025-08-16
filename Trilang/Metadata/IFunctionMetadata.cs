using System.Diagnostics.CodeAnalysis;

namespace Trilang.Metadata;

public interface IFunctionMetadata : IMetadata
{
    ITypeMetadata? DeclaringType { get; }

    [MemberNotNullWhen(false, nameof(DeclaringType))]
    bool IsStatic { get; }

    string Name { get; }

    IReadOnlyList<ParameterMetadata> Parameters { get; }
}