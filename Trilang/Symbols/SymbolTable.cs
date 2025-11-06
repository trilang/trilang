using Trilang.Metadata;

namespace Trilang.Symbols;

public class SymbolTable : ISymbolTable, IEquatable<SymbolTable>
{
    private readonly ISymbolTable parent;
    private readonly ITypeMetadataProvider typeMetadataProvider;
    private readonly Dictionary<string, IdSymbol> variables;

    public SymbolTable(ISymbolTable parent, ITypeMetadataProvider typeMetadataProvider)
    {
        this.parent = parent;
        this.typeMetadataProvider = typeMetadataProvider;
        variables = new Dictionary<string, IdSymbol>();
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

        return variables.DictionaryEquals(other.variables);
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
        => HashCode.Combine(parent, variables);

    public IdSymbol? GetId(string name)
        => variables.TryGetValue(name, out var symbol)
            ? symbol
            : parent.GetId(name);

    public bool TryAddId(IdSymbol symbol)
        => variables.TryAdd(symbol.Name, symbol);

    public void AddType(TypeSymbol symbol)
        => parent.AddType(symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this, typeMetadataProvider.CreateChild());

    public IReadOnlyList<TypeSymbol> Types
        => parent.Types;

    public IReadOnlyDictionary<string, IdSymbol> Ids
        => variables;

    public ITypeMetadataProvider TypeProvider
        => typeMetadataProvider;
}