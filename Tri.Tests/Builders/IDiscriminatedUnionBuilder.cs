using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IDiscriminatedUnionBuilder
{
    IDiscriminatedUnionBuilder AddCase(Func<IInlineTypeBuilder, IInlineTypeNode> action);

    DiscriminatedUnionNode Build();
}