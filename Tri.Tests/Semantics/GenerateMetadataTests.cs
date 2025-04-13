using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class GenerateMetadataTests
{
    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata("main", new FunctionTypeMetadata([], TypeMetadata.Void));
        var function = tree.Declarations[0] as FunctionDeclarationNode;
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetMetadataForIncorrectFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("xxx")
                .Body(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "bool")
                .Body(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata(
            "main",
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void));

        var function = tree.Declarations[0] as FunctionDeclarationNode;
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(function.Parameters[0].Type.Metadata, Is.EqualTo(TypeMetadata.I32));
        Assert.That(function.Parameters[1].Type.Metadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void SetMetadataForIncorrectFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "xxx")
                .Body(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        tree.Accept(new TypeChecker());

        var variable = tree.Find<VariableDeclarationNode>();
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.Type.Metadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void SetMetadataForIncorrectVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "xxx", exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "i32")
                .DefineField("y", "i32")
                .DefineMethod("toString", b => b.Body())
                .DefineMethod("distance", b => b
                    .DefineParameter("other", "i32")
                    .ReturnType("f64")
                    .Body()))
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = tree.Find<TypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("Point").WithValue(TypeSymbol.Type("Point", type)));

        var metadata = store.GetType("Point");
        var expectedMetadata = new TypeMetadata(
            "Point",
            [
                new FieldMetadata(AccessModifierMetadata.Public, "x", TypeMetadata.I32),
                new FieldMetadata(AccessModifierMetadata.Public, "y", TypeMetadata.I32),
            ],
            [
                new MethodMetadata(
                    AccessModifierMetadata.Public,
                    "toString",
                    new FunctionTypeMetadata([], TypeMetadata.Void)),
                new MethodMetadata(
                    AccessModifierMetadata.Public,
                    "distance",
                    new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64)),
            ]);

        Assert.That(metadata, Is.EqualTo(expectedMetadata));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", "i32")
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("MyInt").WithValue(TypeSymbol.Alias("MyInt", type)));

        var metadata = store.GetType("MyInt");
        var expectedMetadata = new TypeAliasMetadata("MyInt", TypeMetadata.I32);
        Assert.That(metadata, Is.EqualTo(expectedMetadata));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunctionType("MyF", builder => builder
                .DefineParameter("i32")
                .DefineParameter("bool")
                .ReturnType("f64"))
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = tree.Find<FunctionTypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("MyF").WithValue(TypeSymbol.Function("MyF", type)));

        var functionTypeMetadata = store.GetType("(i32, bool) => f64");
        var expectedMetadata = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.F64);
        Assert.That(functionTypeMetadata, Is.EqualTo(expectedMetadata));

        var aliasTypeMetadata = store.GetType("MyF");
        var expectedAliasMetadata = new TypeAliasMetadata("MyF", functionTypeMetadata);
        Assert.That(aliasTypeMetadata, Is.EqualTo(expectedAliasMetadata));
    }

    [Test]
    public void SetMetadataForFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("add", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "i32")
                .ReturnType("i32")
                .Body(body => body.Return(exp => exp.Number(0))))
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = tree.Find<FunctionDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key("add").WithValue(new FunctionSymbol(type)));

        var functionTypeMetadata = store.GetType("(i32, i32) => i32");
        var expectedMetadata = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        Assert.That(functionTypeMetadata, Is.EqualTo(expectedMetadata));
    }
}