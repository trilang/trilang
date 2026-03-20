using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Trilang.Metadata;

public class NamespaceMetadata : IMetadata, IEquatable<NamespaceMetadata>
{
    private readonly HashSet<NamespaceMetadata> children;
    private readonly Dictionary<string, ITypeMetadata> types;
    private readonly List<FunctionMetadata> functions;

    private NamespaceMetadata(NamespaceMetadata? parent, string name)
    {
        Parent = parent;
        Name = name;
        children = [];
        types = [];
        functions = [];
    }

    public static NamespaceMetadata CreateRoot(BuiltInTypes builtInTypes)
    {
        var root = new NamespaceMetadata(null, string.Empty);

        root.AddType(builtInTypes.Void.Name, builtInTypes.Void);
        root.AddType(builtInTypes.Null.Name, builtInTypes.Null);

        root.AddType(builtInTypes.I8.Name, builtInTypes.I8);
        root.AddType(builtInTypes.I16.Name, builtInTypes.I16);
        root.AddType(builtInTypes.I32.Name, builtInTypes.I32);
        root.AddType(builtInTypes.I64.Name, builtInTypes.I64);

        root.AddType(builtInTypes.U8.Name, builtInTypes.U8);
        root.AddType(builtInTypes.U16.Name, builtInTypes.U16);
        root.AddType(builtInTypes.U32.Name, builtInTypes.U32);
        root.AddType(builtInTypes.U64.Name, builtInTypes.U64);

        root.AddType(builtInTypes.F32.Name, builtInTypes.F32);
        root.AddType(builtInTypes.F64.Name, builtInTypes.F64);

        root.AddType(builtInTypes.Bool.Name, builtInTypes.Bool);
        root.AddType(builtInTypes.Char.Name, builtInTypes.Char);
        root.AddType(builtInTypes.String.Name, builtInTypes.String);

        return root;
    }

    public static bool operator ==(NamespaceMetadata? left, NamespaceMetadata? right)
        => Equals(left, right);

    public static bool operator !=(NamespaceMetadata? left, NamespaceMetadata? right)
        => !Equals(left, right);

    public bool Equals(NamespaceMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Equals(Parent, other.Parent);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((NamespaceMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Parent);

    public override string ToString()
    {
        if (IsRoot)
            return "Root Namespace";

        if (Parent.IsRoot)
            return Name;

        return $"{Parent}.{Name}";
    }

    private NamespaceMetadata GetRoot()
    {
        var current = this;
        while (!current.IsRoot)
            current = current.Parent;

        return current;
    }

    public NamespaceMetadata CreateChild(IReadOnlyList<string> parts)
    {
        var current = GetRoot();

        foreach (var part in parts)
        {
            var child = current.children.FirstOrDefault(c => c.Name == part);
            if (child is null)
            {
                child = new NamespaceMetadata(current, part);
                current.children.Add(child);
            }

            current = child;
        }

        return current;
    }

    public NamespaceMetadata? FindNamespace(IEnumerable<string> parts)
    {
        var current = GetRoot();

        foreach (var part in parts)
        {
            current = current.children.FirstOrDefault(c => c.Name == part);
            if (current is null)
                return null;
        }

        return current;
    }

    public bool AddType(string name, ITypeMetadata type)
    {
        Debug.Assert(type is not TypeArgumentMetadata, "Generic arguments shouldn't be added a namespace.");
        Debug.Assert(type.Namespace is null, "Type already belongs to another namespace");

        if (!IsRoot && type is IAnonymousTypeMetadata)
            return Parent.AddType(name, type);

        type.Namespace = this;

        return types.TryAdd(name, type);
    }

    public ITypeMetadata? FindType(string name)
        => types.GetValueOrDefault(name);

    public void AddFunction(FunctionMetadata function)
    {
        Debug.Assert(function.Namespace is null, "Function already belongs to another namespace");

        function.Namespace = this;

        functions.Add(function);
    }

    public IEnumerable<FunctionMetadata> FindFunction(string name)
        => functions.Where(f => f.Name == name);

    public IEnumerable<ITypeMetadata> EnumerateAllTypes()
    {
        var q = new Queue<NamespaceMetadata>();
        q.Enqueue(this);

        while (q.TryDequeue(out var current))
        {
            foreach (var type in current.types.Values)
                yield return type;

            foreach (var child in current.children)
                q.Enqueue(child);
        }
    }

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => false;

    public NamespaceMetadata? Parent { get; }

    [MemberNotNullWhen(false, nameof(Parent))]
    public bool IsRoot
        => Parent is null;

    public string Name { get; }

    public IReadOnlyCollection<NamespaceMetadata> Children
        => children;

    public IReadOnlyCollection<ITypeMetadata> Types
        => types.Values;

    public IReadOnlyList<FunctionMetadata> Functions
        => functions;
}