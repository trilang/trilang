using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IInterfaceBuilder
{
    IInterfaceBuilder DefineProperty(
        string name,
        string type,
        AccessModifier getterModifier = AccessModifier.Public,
        AccessModifier setterModifier = AccessModifier.Private);

    IInterfaceBuilder DefineMethod(string name, Action<IInterfaceMethodBuilder> action);

    InterfaceNode Build();
}