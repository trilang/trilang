using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ITypeAliasBuilder
{
    ITypeAliasBuilder AccessModifier(AccessModifier modifier);

    ITypeAliasBuilder DefineType(string name);

    ITypeAliasBuilder DefineFunctionType(Action<IFunctionTypeBuilder> action);

    TypeAliasDeclarationNode Build();
}