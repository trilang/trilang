using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Trilang.Metadata;

public class NamespaceMetadata : IMetadata
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

    public static NamespaceMetadata CreateRoot(BuiltInTypes builtInTypes)
    {
        var root = new NamespaceMetadata(null, string.Empty);

        root.AddType(builtInTypes.Void);
        root.AddType(builtInTypes.Null);

        root.AddType(builtInTypes.I8);
        root.AddType(builtInTypes.I16);
        root.AddType(builtInTypes.I32);
        root.AddType(builtInTypes.I64);

        root.AddType(builtInTypes.U8);
        root.AddType(builtInTypes.U16);
        root.AddType(builtInTypes.U32);
        root.AddType(builtInTypes.U64);

        root.AddType(builtInTypes.F32);
        root.AddType(builtInTypes.F64);

        root.AddType(builtInTypes.Bool);
        root.AddType(builtInTypes.Char);
        root.AddType(builtInTypes.String);

        return root;
    }

    public override string ToString()
    {
        if (IsRoot)
            return "Root Namespace";

        if (Parent.IsRoot)
            return Name;

        return $"{Parent}.{Name}";
    }

    public NamespaceMetadata GetRoot()
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
            if (!current.children.TryGetValue(part, out var child))
            {
                child = new NamespaceMetadata(current, part);
                current.children.Add(child.Name, child);
            }

            current = child;
        }

        return current;
    }

    public NamespaceMetadata? FindNamespace(IEnumerable<string> parts)
    {
        var current = GetRoot();

        foreach (var part in parts)
            if (!current.children.TryGetValue(part, out current))
                return null;

        return current;
    }

    public void AddType(ITypeMetadata type)
    {
        Debug.Assert(type is not TypeArgumentMetadata, "Generic arguments shouldn't be added a namespace.");
        Debug.Assert(type.Namespace is null, "Type already belongs to another namespace");

        if (!IsRoot && type is IAnonymousTypeMetadata)
        {
            Parent.AddType(type);
            return;
        }

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

    [MemberNotNullWhen(false, nameof(Parent))]
    public bool IsRoot
        => Parent is null;

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> Types
        => types;
}