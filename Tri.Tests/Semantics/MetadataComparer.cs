using Trilang.Metadata;

namespace Tri.Tests.Semantics;

internal class MetadataComparer : IEqualityComparer<ITypeMetadata>
{
    public bool Equals(ITypeMetadata? x, ITypeMetadata? y)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null)
            return false;

        if (y is null)
            return false;

        if (x.GetType() != y.GetType())
            return false;

        if (x is TypeAliasMetadata alias1 && y is TypeAliasMetadata alias2)
            return Equals(alias1, alias2);

        if (x is TypeMetadata type1 && y is TypeMetadata type2)
            return Equals(type1, type2);

        return x.Equals(y);
    }

    public int GetHashCode(ITypeMetadata obj)
        => obj.GetHashCode();

    private bool Equals(TypeAliasMetadata x, TypeAliasMetadata y)
        => x.Name == y.Name && Equals(x.Type, y.Type);

    private bool Equals(TypeMetadata x, TypeMetadata y)
    {
        if (x.Name != y.Name)
            return false;

        foreach (var (ga1, ga2) in x.GenericArguments.Zip(y.GenericArguments))
            if (!Equals(ga1, ga2))
                return false;

        foreach (var (i1, i2) in x.Interfaces.Zip(y.Interfaces))
        {
            foreach (var (p1, p2) in i1.Properties.Zip(i2.Properties))
                if (p1.Name != p2.Name && !Equals(p1.Type, p2.Type))
                    return false;

            foreach (var (m1, m2) in i1.Methods.Zip(i2.Methods))
                if (m1.Name != m2.Name && Equals(m1.TypeMetadata, m2.TypeMetadata))
                    return false;
        }

        foreach (var (p1, p2) in x.Properties.Zip(y.Properties))
        {
            if (p1.Name != p2.Name &&
                !Equals(p1.Type, p2.Type) &&
                p1.GetterModifier != p2.GetterModifier &&
                p1.SetterModifier != p2.SetterModifier)
                return false;
        }

        foreach (var (c1, c2) in x.Constructors.Zip(y.Constructors))
        {
            if (c1.AccessModifier != c2.AccessModifier)
                return false;

            foreach (var (p1, p2) in c1.ParameterTypes.Zip(c2.ParameterTypes))
                if (!Equals(p1, p2))
                    return false;
        }

        foreach (var (m1, m2) in x.Methods.Zip(y.Methods))
            if (m1.Name != m2.Name &&
                !Equals(m1.TypeMetadata, m2.TypeMetadata) &&
                m1.AccessModifier != m2.AccessModifier)
                return false;

        return true;
    }
}