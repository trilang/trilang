using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IInterfaceMethodBuilder
{
    IInterfaceMethodBuilder DefineParameter(string name, string type);

    IInterfaceMethodBuilder ReturnType(string type);

    InterfaceMethodNode Build();
}