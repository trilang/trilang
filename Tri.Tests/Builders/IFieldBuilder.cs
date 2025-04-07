using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFieldBuilder
{
    IFieldBuilder AccessModifier(AccessModifier modifier);

    FieldDeclarationNode Build();
}