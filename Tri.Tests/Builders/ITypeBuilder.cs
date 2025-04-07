using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ITypeBuilder
{
    ITypeBuilder AccessModifier(AccessModifier modifier);

    ITypeBuilder DefineField(string name, string type, Action<IFieldBuilder>? action = null);

    ITypeBuilder DefineMethod(string name, Action<IMethodBuilder> action);

    TypeDeclarationNode Build();
}