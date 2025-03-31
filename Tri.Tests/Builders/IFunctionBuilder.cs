using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFunctionBuilder
{
    IFunctionBuilder DefineParameter(string name, string type);

    IFunctionBuilder ReturnType(string type);

    IFunctionBuilder DefineBody(Action<IBlockBuilder> action);

    FunctionDeclarationNode Build();
}