namespace Trilang.Metadata;

public class ParameterMetadata : IMetadata, IEquatable<ParameterMetadata>
{
    public ParameterMetadata(SourceLocation? definition, string name, ITypeMetadata type)
    {
        Definition = definition;
        Name = name;
        Type = type;
    }

    public static bool operator ==(ParameterMetadata? left, ParameterMetadata? right)
        => Equals(left, right);

    public static bool operator !=(ParameterMetadata? left, ParameterMetadata? right)
        => !Equals(left, right);

    public bool Equals(ParameterMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name && Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ParameterMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}