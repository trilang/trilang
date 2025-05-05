using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IInterfaceBuilder
{
    IInterfaceBuilder DefineProperty(string name, string type);

    IInterfaceBuilder DefineMethod(string name, Action<IInterfaceMethodBuilder> action);

    InterfaceNode Build();
}