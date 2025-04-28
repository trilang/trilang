using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IDiscriminatedUnionBuilder
{
    IDiscriminatedUnionBuilder AddType(string type);

    IDiscriminatedUnionBuilder AddFunctionType(Action<IFunctionTypeBuilder>? action = null);

    IDiscriminatedUnionBuilder AddInterface(Action<IInterfaceBuilder>? action = null);

    DiscriminatedUnionNode Build();
}