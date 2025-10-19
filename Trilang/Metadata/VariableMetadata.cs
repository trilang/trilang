namespace Trilang.Metadata;

public class VariableMetadata : IMetadata, IEquatable<VariableMetadata>
{
    public VariableMetadata(
        SourceLocation? definition,
        string name,
        ITypeMetadata type)
    {
        Definition = definition;
        Name = name;
        Type = type;
    }

    public static bool operator ==(VariableMetadata? left, VariableMetadata? right)
        => Equals(left, right);

    public static bool operator !=(VariableMetadata? left, VariableMetadata? right)
        => !Equals(left, right);

    public bool Equals(VariableMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return Name == other.Name &&
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

        return Equals((VariableMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition { get; }

    public bool IsInvalid => false;

    public string Name { get; }

    public ITypeMetadata Type { get; }
}