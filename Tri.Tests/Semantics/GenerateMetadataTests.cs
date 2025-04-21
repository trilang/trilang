using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class GenerateMetadataTests
{
    [Test]
    public void GenerateMetadataForTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "i32")
                .DefineField("y", "i32")
                .DefineConstructor(b => b
                    .DefineParameter("x", "i32")
                    .DefineParameter("y", "i32")
                    .Body(body => body.Return()))
                .DefineMethod("toString", b => b
                    .ReturnType("string")
                    .Body(body => body.Return()))
                .DefineMethod("distance", b => b
                    .AccessModifier(AccessModifier.Private)
                    .DefineParameter("other", "i32")
                    .ReturnType("f64")
                    .Body(body => body.Return())))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expected = new TypeMetadata("Point", [], [], []);
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "x",
            TypeMetadata.I32));
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "y",
            TypeMetadata.I32));
        expected.AddConstructor(new ConstructorMetadata(
            expected,
            AccessModifierMetadata.Public,
            [TypeMetadata.I32, TypeMetadata.I32]));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            "toString",
            new FunctionTypeMetadata([], TypeMetadata.String)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Private,
            "distance",
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64)));

        var actual = provider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected));

        var toStringType = provider.GetType("() => string");
        var expectedToStringType = new FunctionTypeMetadata([], TypeMetadata.String);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType));

        var distanceType = provider.GetType("(i32) => f64");
        var expectedDistanceType = new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType));
    }

    [Test]
    public void GenerateMetadataForTypeMissingFieldTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "xxx"))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("distance", b => b
                    .DefineParameter("other", "xxx")
                    .ReturnType("f64")
                    .Body(body => body.Return())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("toString", b => b
                    .ReturnType("xxx")
                    .Body(body => body.Return())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("i32"))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expected = new TypeAliasMetadata("MyInt", TypeMetadata.I32);

        var actual = provider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("xxx"))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", new TypeNode("i32[]"))
                .Body(body => body.Return()))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expected = new TypeArrayMetadata(TypeMetadata.I32);

        var actual = provider.GetType("i32[]");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", new TypeNode("xxx"))
                .Body(body => body.Return()))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "i32")
                .ReturnType("f64")
                .Body(body => body.Return()))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.F64);

        var actual = provider.GetType("(i32, i32) => f64");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", "xxx")
                .Body(body => body.Return()))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .ReturnType("xxx")
                .Body(body => body.Return()))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new GenerateMetadata()));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", new TypeNode("Point"))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var actualType = provider.GetType("Point");
        var actualAlias = provider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", new TypeNode("Point"))
            .DefineType("Point")
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var actualType = provider.GetType("Point");
        var actualAlias = provider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", new TypeNode("Point[]"))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var actualType = provider.GetType("Point");
        var actualArrayType = provider.GetType("Point[]");
        var actualAlias = provider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", new TypeNode("Point[]"))
            .DefineType("Point")
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var actualType = provider.GetType("Point");
        var actualArrayType = provider.GetType("Point[]");
        var actualAlias = provider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForInterfaceType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .DefineInterface(i => i
                    .DefineField("x", "i32")
                    .DefineField("y", "i32")
                    .DefineMethod("distance", m => m
                        .DefineParameter("other", "Point")
                        .ReturnType("f64"))))
            .Build();

        var provider = new TypeMetadataProvider();
        var generator = new GenerateMetadata(provider);
        tree.Accept(generator);

        var expectedInterface = new InterfaceMetadata("{ x: i32; y: i32; distance(Point): f64; }");
        var expectedAlias = new TypeAliasMetadata("Point", expectedInterface);

        var actualInterface = provider.GetType(expectedInterface.Name);
        Assert.That(actualInterface, Is.EqualTo(expectedInterface));

        var actualAlias = provider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }
}