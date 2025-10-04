namespace Trilang.Parsing.Ast;

public interface IHasGenericArguments
{
    IReadOnlyList<IInlineTypeNode> GenericArguments { get; }
}