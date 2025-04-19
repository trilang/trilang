using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
{
    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(_ => { }))
            .Build();

        var provider = new TypeMetadataProvider();
        var functionType = new FunctionTypeMetadata([], TypeMetadata.Void);
        provider.DefineType(functionType);

        tree.Accept(new TypeChecker(provider));

        var expected = new FunctionMetadata("main", new FunctionTypeMetadata([], TypeMetadata.Void));
        var function = tree.Find<FunctionDeclarationNode>();
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

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
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

        var provider = new TypeMetadataProvider();
        var functionType = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void);
        provider.DefineType(functionType);

        tree.Accept(new TypeChecker(provider));

        var expected = new FunctionMetadata(
            "main",
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void));

        var function = tree.Find<FunctionDeclarationNode>();
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

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        tree.Accept(new TypeChecker(provider));

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

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
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

        var provider = new TypeMetadataProvider();
        var typeMetadata = new TypeMetadata(
            "Point",
            [
            ],
            [],
            [
            ]);
        typeMetadata.AddField(new FieldMetadata(
            typeMetadata,
            AccessModifierMetadata.Public,
            "x",
            TypeMetadata.I32));
        typeMetadata.AddField(new FieldMetadata(
            typeMetadata,
            AccessModifierMetadata.Public,
            "y",
            TypeMetadata.I32));
        typeMetadata.AddMethod(new MethodMetadata(
            typeMetadata,
            AccessModifierMetadata.Public,
            "toString",
            new FunctionTypeMetadata([], TypeMetadata.Void)));
        typeMetadata.AddMethod(new MethodMetadata(
            typeMetadata,
            AccessModifierMetadata.Public,
            "distance",
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64)));

        provider.DefineType(typeMetadata);

        tree.Accept(new TypeChecker(provider));

        var type = tree.Find<TypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(typeMetadata));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("i32"))
            .Build();

        var provider = new TypeMetadataProvider();
        var aliasMetadata = new TypeAliasMetadata("MyInt", TypeMetadata.I32);
        provider.DefineType(aliasMetadata);
        tree.Accept(new TypeChecker(provider));

        var node = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(aliasMetadata));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyF", builder => builder
                .DefineFunctionType(f => f
                    .DefineParameter("i32")
                    .DefineParameter("bool")
                    .ReturnType("f64")))
            .Build();

        var provider = new TypeMetadataProvider();
        var functionType = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.F64);
        provider.DefineType(functionType);
        var aliasMetadata = new TypeAliasMetadata("MyF", functionType);
        provider.DefineType(aliasMetadata);

        tree.Accept(new TypeChecker(provider));

        var type = tree.Find<FunctionTypeNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(functionType));
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

        var provider = new TypeMetadataProvider();
        var functionType = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        provider.DefineType(functionType);

        tree.Accept(new TypeChecker(provider));

        var node = tree.Find<FunctionDeclarationNode>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(new FunctionMetadata("add", functionType)));
    }

    [Test]
    public void LiteralNumberTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.True())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Bool));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void LiteralCharTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("char")
                .Body(body => body
                    .Return(exp => exp.Char('x'))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Char));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Char));
    }

    [Test]
    public void LiteralStringTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("string")
                .Body(body => body
                    .Return(exp => exp.String("xxx"))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.String));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.String));
    }

    [Test]
    public void ReturnStatementDoesntTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.Number(1))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Bool));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void UnaryPlusTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LogicalNotTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.True().LogicalNot())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Bool));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).Number(2).Add())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        tree.Accept(new TypeChecker(provider));

        var binaryNode = tree.Find<BinaryExpressionNode>();
        Assert.That(binaryNode, Is.Not.Null);
        Assert.That(binaryNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LogicalNotIncorrectOperandTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).LogicalNot())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Variable("a"))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([TypeMetadata.I32,], TypeMetadata.I32));

        tree.Accept(new TypeChecker(provider));

        var returnNode = tree.Find<ReturnStatementNode>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.True())))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .If(exp => exp.Number(1), _ => { })))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void FunctionCallIncorrectParameterTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("add", builder => builder
                .DefineParameter("a", "i32")
                .ReturnType("i32")
                .Body(_ => { }))
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.True().Call("add"))))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32));
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.I32));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .While(exp => exp.Number(1), _ => { })))
            .Build();

        var provider = new TypeMetadataProvider();
        provider.DefineType(new FunctionTypeMetadata([], TypeMetadata.Void));

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void ReturnInConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(ctor => ctor
                    .Body(body => body
                        .Return())))
            .Build();

        var provider = new TypeMetadataProvider();
        var typeMetadata = new TypeMetadata("Point", [], [], []);
        var constructorMetadata = new ConstructorMetadata(typeMetadata, AccessModifierMetadata.Public, []);
        typeMetadata.AddConstructor(constructorMetadata);
        provider.DefineType(typeMetadata);

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }

    [Test]
    public void ReturnWithExpressionInConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(ctor => ctor
                    .Body(body => body
                        .Return(exp => exp.Number(0)))))
            .Build();

        var provider = new TypeMetadataProvider();
        var typeMetadata = new TypeMetadata("Point", [], [], []);
        var constructorMetadata = new ConstructorMetadata(typeMetadata, AccessModifierMetadata.Public, []);
        typeMetadata.AddConstructor(constructorMetadata);
        provider.DefineType(typeMetadata);

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker(provider)));
    }
}