using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ITypeAliasBuilder
{
    ITypeAliasBuilder AccessModifier(AccessModifier modifier);

    ITypeAliasBuilder Type(string name);

    ITypeAliasBuilder FunctionType(Action<IFunctionTypeBuilder> action);

    ITypeAliasBuilder Interface(Action<IInterfaceBuilder>? action = null);

    ITypeAliasBuilder DiscriminatedUnion(Action<IDiscriminatedUnionBuilder> action);

    ITypeAliasBuilder Tuple(Action<ITupleBuilder> action);

    TypeAliasDeclarationNode Build();
}