using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IFunctionTypeBuilder
{
    IFunctionTypeBuilder DefineParameter(string type);

    IFunctionTypeBuilder DefineParameter(Func<IInlineTypeBuilder, IInlineTypeNode> action);

    IFunctionTypeBuilder ReturnType(string type);
}