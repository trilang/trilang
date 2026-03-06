namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata, IEquatable<PropertyMetadata>
{
    public PropertyMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        MethodMetadata? getter,
        MethodMetadata? setter)
    {
        Definition = definition;
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;
    }

    public static bool operator ==(PropertyMetadata? left, PropertyMetadata? right)
        => Equals(left, right);

    public static bool operator !=(PropertyMetadata? left, PropertyMetadata? right)
        => !Equals(left, right);

    public bool Equals(PropertyMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return Equals(DeclaringType, other.DeclaringType) &&
               Name == other.Name &&
               Type.Equals(other.Type) &&
               Getter == other.Getter &&
               Setter == other.Setter;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type, Getter, Setter);

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public MethodMetadata? Getter { get; }

    public MethodMetadata? Setter { get; }

    public ITypeMetadata Type { get; }
}