using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IGenericTypeBuilder
{
    IGenericTypeBuilder DefineGenericArgument(string name);

    IGenericTypeBuilder DefineGenericArgument(Func<IInlineTypeBuilder, IInlineTypeNode> action);

    GenericTypeNode Build();
}