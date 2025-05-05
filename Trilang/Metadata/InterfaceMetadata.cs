namespace Trilang.Metadata;

public class InterfaceMetadata : ITypeMetadata, IEquatable<InterfaceMetadata>
{
    private readonly HashSet<InterfacePropertyMetadata> properties;
    private readonly HashSet<InterfaceMethodMetadata> methods;

    public InterfaceMetadata(string name) : this(name, [], [])
    {
    }

    public InterfaceMetadata(
        string name,
        IEnumerable<InterfacePropertyMetadata> properties,
        IEnumerable<InterfaceMethodMetadata> methods)
    {
        Name = name;
        this.properties = new HashSet<InterfacePropertyMetadata>(properties);
        this.methods = new HashSet<InterfaceMethodMetadata>(methods);
    }

    public static bool operator ==(InterfaceMetadata? left, InterfaceMetadata? right)
        => Equals(left, right);

    public static bool operator !=(InterfaceMetadata? left, InterfaceMetadata? right)
        => !Equals(left, right);

    public bool Equals(InterfaceMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((InterfaceMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public void AddProperty(InterfacePropertyMetadata property)
    {
        if (!properties.Add(property))
            throw new ArgumentException($"Property with name {property.Name} already exists in type {Name}");
    }

    public InterfacePropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddMethod(InterfaceMethodMetadata method)
    {
        if (!methods.Add(method))
            throw new ArgumentException($"Method with name {method.Name} already exists in type {Name}");
    }

    public InterfaceMethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(m => m.Name == name);

    public string Name { get; }

    public IReadOnlyCollection<InterfacePropertyMetadata> Properties => properties;

    public IReadOnlyCollection<InterfaceMethodMetadata> Methods => methods;
}