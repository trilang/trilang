namespace Trilang.Metadata;

public class ConstructorMetadata : IFunctionMetadata, IEquatable<ConstructorMetadata>
{
    public ConstructorMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        Parameters = parameters;
        Type = type;
    }

    public static ConstructorMetadata Invalid()
        => new ConstructorMetadata(
            null,
            TypeMetadata.InvalidType,
            AccessModifierMetadata.Public,
            [],
            FunctionTypeMetadata.Invalid());

    public static bool operator ==(ConstructorMetadata? left, ConstructorMetadata? right)
        => Equals(left, right);

    public static bool operator !=(ConstructorMetadata? left, ConstructorMetadata? right)
        => !Equals(left, right);

    public bool Equals(ConstructorMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               Parameters.SequenceEqual(other.Parameters) &&
               Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ConstructorMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(AccessModifier, Parameters, Type);

    public override string ToString()
        => $"ctor: {Type}";

    public SourceLocation? Definition { get; }

    public bool IsInvalid => false;

    public ITypeMetadata DeclaringType { get; }

    public string Name => "ctor";

    public AccessModifierMetadata AccessModifier { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }
}