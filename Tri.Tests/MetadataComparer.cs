using Trilang.Metadata;

namespace Tri.Tests;

// ReSharper disable all UnusedParameter.Local
internal class MetadataComparer : IEqualityComparer<IMetadata>
{
    private readonly HashSet<(IMetadata, IMetadata)> seen = [];

    public bool Equals(IMetadata? x, IMetadata? y)
    {
        if (x == null && y == null)
            return true;

        if (x == null)
            throw new Exception("x is null");

        if (y == null)
            throw new Exception("y is null");

        if (!seen.Add((x, y)))
            return true;

        return (x, y) switch
        {
            (ConstructorMetadata x1, ConstructorMetadata y1)
                => CompareConstructorMetadata(x1, y1),
            (DiscriminatedUnionMetadata x1, DiscriminatedUnionMetadata y1)
                => CompareDiscriminatedUnionMetadata(x1, y1),
            (FieldMetadata x1, FieldMetadata y1)
                => CompareFieldMetadata(x1, y1),
            (FunctionMetadata x1, FunctionMetadata y1)
                => CompareFunctionMetadata(x1, y1),
            (FunctionTypeMetadata x1, FunctionTypeMetadata y1)
                => CompareFunctionTypeMetadata(x1, y1),
            (InterfaceMetadata x1, InterfaceMetadata y1)
                => CompareInterfaceMetadata(x1, y1),
            (InterfaceMethodMetadata x1, InterfaceMethodMetadata y1)
                => CompareInterfaceMethodMetadata(x1, y1),
            (InterfacePropertyMetadata x1, InterfacePropertyMetadata y1)
                => CompareInterfacePropertyMetadata(x1, y1),
            (MethodMetadata x1, MethodMetadata y1)
                => CompareMethodMetadata(x1, y1),
            (ParameterMetadata x1, ParameterMetadata y1)
                => CompareParameterMetadata(x1, y1),
            (PropertyMetadata x1, PropertyMetadata y1)
                => ComparePropertyMetadata(x1, y1),
            (TupleMetadata x1, TupleMetadata y1)
                => CompareTupleMetadata(x1, y1),
            (TypeAliasMetadata x1, TypeAliasMetadata y1)
                => CompareTypeAliasMetadata(x1, y1),
            (TypeArgumentMetadata x1, TypeArgumentMetadata y1)
                => CompareTypeArgumentMetadata(x1, y1),
            (TypeArrayMetadata x1, TypeArrayMetadata y1)
                => CompareTypeArrayMetadata(x1, y1),
            (TypeMetadata x1, TypeMetadata y1)
                => CompareTypeMetadata(x1, y1),
            (VariableMetadata x1, VariableMetadata y1)
                => CompareVariableMetadata(x1, y1),

            _ => throw new Exception($"{x.GetType()} != {y.GetType()}"),
        };
    }

    private bool CompareConstructorMetadata(ConstructorMetadata x, ConstructorMetadata y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TypeMetadata doesn't match.");

        return true;
    }

    private bool CompareDiscriminatedUnionMetadata(DiscriminatedUnionMetadata x, DiscriminatedUnionMetadata y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Types don't match.");

        return true;
    }

    private bool CompareFieldMetadata(FieldMetadata x, FieldMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        return true;
    }

    private bool CompareFunctionMetadata(FunctionMetadata x, FunctionMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TypeMetadata doesn't match.");

        return true;
    }

    private bool CompareFunctionTypeMetadata(FunctionTypeMetadata x, FunctionTypeMetadata y)
    {
        if (!x.ParameterTypes.SequenceEqual(y.ParameterTypes, this))
            throw new Exception("ParameterTypes don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        return true;
    }

    private bool CompareInterfaceMetadata(InterfaceMetadata x, InterfaceMetadata y)
    {
        if (!x.Properties.SequenceEqual(y.Properties, this))
            throw new Exception("Properties don't match.");

        if (!x.Methods.SequenceEqual(y.Methods, this))
            throw new Exception("Methods don't match.");

        return true;
    }

    private bool CompareInterfaceMethodMetadata(InterfaceMethodMetadata x, InterfaceMethodMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TypeMetadata doesn't match.");

        return true;
    }

    private bool CompareInterfacePropertyMetadata(InterfacePropertyMetadata x, InterfacePropertyMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (x.GetterModifier != y.GetterModifier)
            throw new Exception($"GetterModifier doesn't match. {x.GetterModifier} != {y.GetterModifier}.");

        if (x.SetterModifier != y.SetterModifier)
            throw new Exception($"SetterModifier doesn't match. {x.SetterModifier} != {y.SetterModifier}.");

        return true;
    }

    private bool CompareMethodMetadata(MethodMetadata x, MethodMetadata y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.IsStatic != y.IsStatic)
            throw new Exception($"IsStatic doesn't match. {x.IsStatic} != {y.IsStatic}.");

        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TypeMetadata doesn't match.");

        return true;
    }

    private bool CompareParameterMetadata(ParameterMetadata x, ParameterMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        return true;
    }

    private bool ComparePropertyMetadata(PropertyMetadata x, PropertyMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!Equals(x.Getter, y.Getter))
            throw new Exception("Getter doesn't match.");

        if (!Equals(x.Setter, y.Setter))
            throw new Exception("Setter doesn't match.");

        return true;
    }

    private bool CompareTupleMetadata(TupleMetadata x, TupleMetadata y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Types don't match.");

        return true;
    }

    private bool CompareTypeAliasMetadata(TypeAliasMetadata x, TypeAliasMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.GenericArguments.SequenceEqual(y.GenericArguments, this))
            throw new Exception("GenericArguments don't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        return true;
    }

    private bool CompareTypeArgumentMetadata(TypeArgumentMetadata x, TypeArgumentMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        return true;
    }

    private bool CompareTypeArrayMetadata(TypeArrayMetadata x, TypeArrayMetadata y)
    {
        if (!Equals(x.ItemMetadata, y.ItemMetadata))
            throw new Exception("ItemMetadata doesn't match.");

        return true;
    }

    private bool CompareTypeMetadata(TypeMetadata x, TypeMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.GenericArguments.SequenceEqual(y.GenericArguments, this))
            throw new Exception("GenericArguments don't match.");

        if (!x.Interfaces.SequenceEqual(y.Interfaces, this))
            throw new Exception("Interfaces don't match.");

        if (!x.Fields.SequenceEqual(y.Fields, this))
            throw new Exception("Fields don't match.");

        if (!x.Properties.SequenceEqual(y.Properties, this))
            throw new Exception("Properties don't match.");

        if (!x.Constructors.SequenceEqual(y.Constructors, this))
            throw new Exception("Constructors don't match.");

        if (!x.Methods.SequenceEqual(y.Methods, this))
            throw new Exception("Methods don't match.");

        return true;
    }

    private bool CompareVariableMetadata(VariableMetadata x, VariableMetadata y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        return true;
    }

    public int GetHashCode(IMetadata obj)
        => obj.GetHashCode();
}