using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IInlineTypeBuilder
{
    IInlineTypeNode Type(string name);

    IInlineTypeNode Array(string name);

    IInlineTypeNode FunctionType(Action<IFunctionTypeBuilder> action);

    IInlineTypeNode Interface(Action<IInterfaceBuilder>? action = null);

    IInlineTypeNode DiscriminatedUnion(Action<IDiscriminatedUnionBuilder> action);

    IInlineTypeNode Tuple(Action<ITupleBuilder> action);

    IInlineTypeNode Generic(string name, Action<IGenericTypeBuilder> action);
}