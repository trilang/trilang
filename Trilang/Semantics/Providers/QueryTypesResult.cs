using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public readonly struct QueryTypesResult
{
    public IReadOnlyList<ITypeMetadata> Types { get; }

    public QueryTypesReason Reason { get; }

    private QueryTypesResult(IReadOnlyList<ITypeMetadata> types, QueryTypesReason reason)
    {
        Types = types;
        Reason = reason;
    }

    public static QueryTypesResult Success(IReadOnlyList<ITypeMetadata> types)
        => new QueryTypesResult(types, QueryTypesReason.Success);

    public static QueryTypesResult TypeNotFound()
        => new QueryTypesResult([], QueryTypesReason.TypeNotFound);

    public static QueryTypesResult MultipleTypesFound(IReadOnlyList<ITypeMetadata> types)
        => new QueryTypesResult(types, QueryTypesReason.MultipleTypesFound);

    public static QueryTypesResult NamespaceNotFound()
        => new QueryTypesResult([], QueryTypesReason.NamespaceNotFound);

    public bool IsSuccess
        => Reason == QueryTypesReason.Success;

    public bool IsTypeNotFound
        => Reason == QueryTypesReason.TypeNotFound;

    public bool IsMultipleTypesFound
        => Reason == QueryTypesReason.MultipleTypesFound;

    public bool IsNamespaceNotFound
        => Reason == QueryTypesReason.NamespaceNotFound;
}