using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public class RootSymbolTable : ISymbolTable, IEquatable<RootSymbolTable>
{
    private readonly List<TypeSymbol> types;
    private readonly Dictionary<string, IdSymbol> ids;

    public RootSymbolTable()
    {
        types = [];
        ids = [];
    }

    public static bool operator ==(RootSymbolTable? left, RootSymbolTable? right)
        => Equals(left, right);

    public static bool operator !=(RootSymbolTable? left, RootSymbolTable? right)
        => !Equals(left, right);

    public bool Equals(RootSymbolTable? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return types.SequenceEqual(other.types) &&
               ids.DictionaryEquals(other.ids);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((RootSymbolTable)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types, ids);

    public IdSymbol? GetId(string name)
        => ids.GetValueOrDefault(name);

    public void AddId(string name, ISemanticNode node)
    {
        if (ids.TryGetValue(name, out var symbol))
            symbol.AddNode(node);
        else
            ids.Add(name, new IdSymbol(name, node));
    }

    public void AddType(TypeSymbol symbol)
        => types.Add(symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyList<TypeSymbol> Types
        => types;

    public IReadOnlyDictionary<string, IdSymbol> Ids
        => ids;
}