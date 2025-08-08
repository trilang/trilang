namespace Trilang.Metadata;

public class InterfaceMetadata : ITypeMetadata, IEquatable<InterfaceMetadata>
{
    private readonly HashSet<InterfacePropertyMetadata> properties;
    private readonly HashSet<InterfaceMethodMetadata> methods;

    public InterfaceMetadata() : this([], [])
    {
    }

    public InterfaceMetadata(
        IEnumerable<InterfacePropertyMetadata> properties,
        IEnumerable<InterfaceMethodMetadata> methods)
    {
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

        foreach (var (p1, p2) in properties.Zip(other.properties))
        {
            if (p1.Name != p2.Name || !p1.Type.Equals(p2.Type))
                return false;
        }

        foreach (var (m1, m2) in methods.Zip(other.methods))
        {
            if (m1.Name != m2.Name || !m1.TypeMetadata.Equals(m2.TypeMetadata))
                return false;
        }

        return true;
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
        => HashCode.Combine(properties, methods);

    public override string ToString()
    {
        var propertyNames = properties.Select(f => $"{f.Name}: {f.Type};");
        var methodNames = methods.Select(m => $"{m.Name}({string.Join(", ", m.TypeMetadata.ParameterTypes)}): {m.TypeMetadata.ReturnType};");

        var combinedSignatures = propertyNames.Concat(methodNames).ToList();

        return combinedSignatures.Any()
            ? $"{{ {string.Join(" ", combinedSignatures)} }}"
            : "{ }";
    }

    public void AddProperty(InterfacePropertyMetadata property)
    {
        if (!properties.Add(property))
            throw new ArgumentException($"Property with name {property.Name} already exists in interface.");
    }

    public InterfacePropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddMethod(InterfaceMethodMetadata method)
    {
        if (!methods.Add(method))
            throw new ArgumentException($"Method with name {method.Name} already exists in interface.");
    }

    public InterfaceMethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(m => m.Name == name);

    public IMetadata? GetMember(string name)
        => GetProperty(name) ??
           GetMethod(name) as IMetadata;

    public IReadOnlyCollection<InterfacePropertyMetadata> Properties
        => properties;

    public IReadOnlyCollection<InterfaceMethodMetadata> Methods
        => methods;

    public bool IsValueType
        => false;
}