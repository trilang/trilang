using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IPropertyBuilder
{
    IPropertyBuilder AccessModifier(AccessModifier modifier);

    PropertyDeclarationNode Build();
}