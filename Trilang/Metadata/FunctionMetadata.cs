namespace Trilang.Metadata;

public class FunctionMetadata : IFunctionMetadata, IEquatable<FunctionMetadata>
{
    public FunctionMetadata(
        SourceLocation? definition,
        AccessModifierMetadata accessModifier,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        AccessModifier = accessModifier;
        Name = name;
        Parameters = parameters;
        Type = type;
    }

    public static bool operator ==(FunctionMetadata? left, FunctionMetadata? right)
        => Equals(left, right);

    public static bool operator !=(FunctionMetadata? left, FunctionMetadata? right)
        => !Equals(left, right);

    public bool Equals(FunctionMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
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

        return Equals((FunctionMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Parameters, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition { get; }

    public bool IsInvalid => false;

    public ITypeMetadata? DeclaringType => null;

    public bool IsStatic => true;

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }
}