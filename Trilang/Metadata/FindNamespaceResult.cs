using System.Diagnostics.CodeAnalysis;

namespace Trilang.Metadata;

public readonly struct FindNamespaceResult
{
    public FindNamespaceResult(NamespaceMetadata? @namespace, FindNamespaceReason reason)
    {
        Namespace = @namespace;
        Reason = reason;
    }

    public static FindNamespaceResult Success(NamespaceMetadata @namespace)
        => new FindNamespaceResult(@namespace, FindNamespaceReason.Success);

    public static FindNamespaceResult NamespaceNotFound()
        => new FindNamespaceResult(null, FindNamespaceReason.NamespaceNotFound);

    public static FindNamespaceResult PackageNotFound()
        => new FindNamespaceResult(null, FindNamespaceReason.PackageNotFound);

    [MemberNotNullWhen(true, nameof(Namespace))]
    public bool IsSuccess
        => Reason == FindNamespaceReason.Success;

    public bool IsNamespaceNotFound
        => Reason == FindNamespaceReason.NamespaceNotFound;

    public bool IsPackageNotFound
        => Reason == FindNamespaceReason.PackageNotFound;

    public NamespaceMetadata? Namespace { get; }

    public FindNamespaceReason Reason { get; }
}