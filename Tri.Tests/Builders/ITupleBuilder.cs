using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ITupleBuilder
{
    ITupleBuilder AddCase(Func<IInlineTypeBuilder, IInlineTypeNode> action);

    TupleTypeNode Build();
}