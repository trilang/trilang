using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ITypeBuilder
{
    ITypeBuilder AccessModifier(AccessModifier modifier);

    ITypeBuilder DefineProperty(string name, string type, Action<IPropertyBuilder>? action = null);

    ITypeBuilder DefineProperty(
        string name,
        Func<IInlineTypeBuilder, IInlineTypeNode> type,
        Action<IPropertyBuilder>? action = null);

    ITypeBuilder DefineConstructor(Action<IConstructorBuilder> action);

    ITypeBuilder DefineMethod(string name, Action<IMethodBuilder> action);

    ITypeBuilder AddInterface(string name);

    ITypeBuilder DefineGenericArgument(string name);

    TypeDeclarationNode Build();
}