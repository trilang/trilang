namespace Trilang.Metadata;

public class InterfaceMetadata : ITypeMetadata, IEquatable<InterfaceMetadata>
{
    private readonly HashSet<InterfaceFieldMetadata> fields;
    private readonly HashSet<InterfaceMethodMetadata> methods;

    public InterfaceMetadata(string name) : this(name, [], [])
    {
    }

    public InterfaceMetadata(
        string name,
        IEnumerable<InterfaceFieldMetadata> fields,
        IEnumerable<InterfaceMethodMetadata> methods)
    {
        Name = name;
        this.fields = new HashSet<InterfaceFieldMetadata>(fields);
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

    public void AddField(InterfaceFieldMetadata field)
    {
        if (!fields.Add(field))
            throw new ArgumentException($"Field with name {field.Name} already exists in type {Name}");
    }

    public InterfaceFieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void AddMethod(InterfaceMethodMetadata method)
    {
        if (!methods.Add(method))
            throw new ArgumentException($"Method with name {method.Name} already exists in type {Name}");
    }

    public InterfaceMethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(m => m.Name == name);

    public string Name { get; }

    public IReadOnlyCollection<InterfaceFieldMetadata> Fields => fields;

    public IReadOnlyCollection<InterfaceMethodMetadata> Methods => methods;
}