using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public class SymbolTable : ISymbolTable, IEquatable<SymbolTable>
{
    private readonly ISymbolTable parent;
    private readonly ITypeMetadataProvider typeMetadataProvider;
    private readonly Dictionary<string, IdSymbol> ids;

    public SymbolTable(ISymbolTable parent, ITypeMetadataProvider typeMetadataProvider)
    {
        this.parent = parent;
        this.typeMetadataProvider = typeMetadataProvider;
        ids = [];
    }

    public static bool operator ==(SymbolTable? left, SymbolTable? right)
        => Equals(left, right);

    public static bool operator !=(SymbolTable? left, SymbolTable? right)
        => !Equals(left, right);

    public bool Equals(SymbolTable? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ids.DictionaryEquals(other.ids);
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
        => HashCode.Combine(parent, ids);

    public IdSymbol? GetId(string name)
        => ids.TryGetValue(name, out var symbol)
            ? symbol
            : parent.GetId(name);

    public void AddId(string name, ISemanticNode node)
    {
        if (ids.TryGetValue(name, out var symbol))
            symbol.AddNode(node);
        else
            ids.Add(name, new IdSymbol(name, node));
    }

    public void AddType(TypeSymbol symbol)
        => parent.AddType(symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this, typeMetadataProvider.CreateChild());

    public IReadOnlyList<TypeSymbol> Types
        => parent.Types;

    public IReadOnlyDictionary<string, IdSymbol> Ids
        => ids;

    public ITypeMetadataProvider TypeProvider
        => typeMetadataProvider;
}