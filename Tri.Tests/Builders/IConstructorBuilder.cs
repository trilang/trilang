using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IConstructorBuilder
{
    IConstructorBuilder AccessModifier(AccessModifier modifier);

    IConstructorBuilder DefineParameter(string name, string type);

    IConstructorBuilder DefineParameter(string name, TypeNode type);

    IConstructorBuilder Body(Action<IBlockBuilder>? action = null);

    ConstructorDeclarationNode Build();
}