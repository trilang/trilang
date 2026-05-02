using System.Diagnostics;

namespace Trilang.Metadata;

public class NamespaceMetadata : INamespaceMetadata
{
    private readonly Dictionary<string, NamespaceMetadata> children;
    private readonly List<ITypeMetadata> types; // TODO: hashset/dictionary?
    private readonly List<FunctionMetadata> functions;

    private NamespaceMetadata(NamespaceMetadata? parent, string name)
    {
        Parent = parent;
        Name = name;
        children = [];
        types = [];
        functions = [];
    }

    public static NamespaceMetadata CreateForPackage()
        => new NamespaceMetadata(null, string.Empty);

    public override string ToString()
    {
        if (Parent?.Parent is null)
            return Name;

        return $"{Parent}.{Name}";
    }

    public NamespaceMetadata CreateChild(IReadOnlyList<string> parts)
    {
        var current = this;

        foreach (var part in parts)
        {
            if (!current.children.TryGetValue(part, out var child))
            {
                child = new NamespaceMetadata(current, part);
                current.children.Add(child.Name, child);
            }

            current = child;
        }

        return current;
    }

    public NamespaceMetadata? GetNamespace(string part)
        => children.GetValueOrDefault(part);

    public void AddType(ITypeMetadata type)
    {
        Debug.Assert(type is not TypeArgumentMetadata, "Generic arguments shouldn't be added a namespace.");
        Debug.Assert(type is not IAnonymousTypeMetadata, "Anonymous types can only be added to root namespace.");
        Debug.Assert(type.Namespace is null, "Type already belongs to another namespace.");

        type.Namespace = this;

        types.Add(type);
    }

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
            foreach (var type in current.types)
                yield return type;

            foreach (var (_, child) in current.children)
                q.Enqueue(child);
        }
    }

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => false;

    public NamespaceMetadata? Parent { get; }

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> Types
        => types;
}