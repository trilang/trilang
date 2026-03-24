namespace Trilang.Semantics.Model;

public interface IGenericDeclaration : IDeclaration
{
    string Name { get; }

    IReadOnlyList<TypeRef> GenericArguments { get; }

    bool IsGeneric { get; }
}