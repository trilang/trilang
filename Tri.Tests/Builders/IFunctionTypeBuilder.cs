using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFunctionTypeBuilder
{
    IFunctionTypeBuilder AccessModifier(AccessModifier modifier);

    IFunctionTypeBuilder DefineParameter(string type);

    IFunctionTypeBuilder ReturnType(string type);
}