using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IPropertyBuilder
{
    IPropertyBuilder AccessModifier(AccessModifier modifier);

    IPropertyBuilder Getter(AccessModifier modifier, Action<IBlockBuilder>? action = null);

    IPropertyBuilder Setter(AccessModifier modifier, Action<IBlockBuilder>? action = null);

    PropertyDeclarationNode Build();
}