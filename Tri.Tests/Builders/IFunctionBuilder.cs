using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFunctionBuilder
{
    IFunctionBuilder DefineParameter(string name, Func<IInlineTypeBuilder, IInlineTypeNode> action);

    IFunctionBuilder ReturnType(string type);

    IFunctionBuilder ReturnType(Func<IInlineTypeBuilder, IInlineTypeNode> action);

    IFunctionBuilder Body(Action<IBlockBuilder> action);

    FunctionDeclarationNode Build();
}