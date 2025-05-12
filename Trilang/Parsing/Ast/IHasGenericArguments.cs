namespace Trilang.Parsing.Ast;

public interface IHasGenericArguments
{
    IReadOnlyList<TypeNode> GenericArguments { get; }
}