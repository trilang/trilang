using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFunctionBuilder
{
    IFunctionBuilder DefineParameter(string name, string type);

    IFunctionBuilder DefineParameter(string name, TypeNode type);

    IFunctionBuilder ReturnType(string type);

    IFunctionBuilder Body(Action<IBlockBuilder> action);

    FunctionDeclarationNode Build();
}