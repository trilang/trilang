using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class SymbolFinderTests
{
    [Test]
    public void FunctionInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var function = FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));
    }

    [Test]
    public void TwoFunctionsInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var function1 = FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode());
        var function2 = FunctionDeclarationNode.Create("add", [], new TypeNode("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function1, function2]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(2));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function1.Name).WithValue(new FunctionSymbol(function1)));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function2.Name).WithValue(new FunctionSymbol(function2)));
    }

    [Test]
    public void SameFunctionInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var function1 = FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode());
        var function2 = FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function1, function2]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void FunctionWithParametersInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new ParameterNode("a", new TypeNode("i32"));
        var b = new ParameterNode("b", new TypeNode("i32"));
        var function = FunctionDeclarationNode.Create(
            "add",
            [a, b],
            new TypeNode("void"),
            new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key("add").WithValue(new FunctionSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.VariablesInScope, Has.Count.EqualTo(2));
        Assert.That(function.Body.SymbolTable.VariablesInScope, Contains.Key(a.Name).WithValue(new VariableSymbol(a)));
        Assert.That(function.Body.SymbolTable.VariablesInScope, Contains.Key(b.Name).WithValue(new VariableSymbol(b)));
    }

    [Test]
    public void FunctionWithSameParametersInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new ParameterNode("a", new TypeNode("i32"));
        var b = new ParameterNode("a", new TypeNode("i32"));
        var function = FunctionDeclarationNode.Create(
            "add",
            [a, b],
            new TypeNode("void"),
            new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void FunctionWithVariablesInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1));
        var b = new VariableDeclarationStatementNode("b", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2));
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([a, b]));
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.VariablesInScope, Has.Count.EqualTo(2));
        Assert.That(function.Body.SymbolTable.VariablesInScope, Contains.Key(a.Name).WithValue(new VariableSymbol(a)));
        Assert.That(function.Body.SymbolTable.VariablesInScope, Contains.Key(b.Name).WithValue(new VariableSymbol(b)));
    }

    [Test]
    public void FunctionWithSameVariablesInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1));
        var b = new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2));
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([a, b]));
        var tree = new SyntaxTree([function]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void IfScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([
                a,
            ])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Contains.Key(a.Name).WithValue(new VariableSymbol(a)));
    }

    [Test]
    public void IfElseScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var b = new VariableDeclarationStatementNode("b", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([a]),
            new BlockStatementNode([b])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Contains.Key(a.Name).WithValue(new VariableSymbol(a)));

        Assert.That(ifStatement.Else, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Else.SymbolTable.VariablesInScope, Contains.Key(b.Name).WithValue(new VariableSymbol(b)));
    }

    [Test]
    public void SameVariableInMultipleScopesTest()
    {
        var builder = new SymbolFinder();
        var a1 = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var a2 = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var a3 = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([a2]),
            new BlockStatementNode([a3])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([a1, ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.FunctionsInScope, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.FunctionsInScope, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(function.Body.SymbolTable.VariablesInScope, Contains.Key(a1.Name).WithValue(new VariableSymbol(a1)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Contains.Key(a2.Name).WithValue(new VariableSymbol(a2)));

        Assert.That(ifStatement.Else, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Else.SymbolTable.VariablesInScope, Contains.Key(a3.Name).WithValue(new VariableSymbol(a3)));
    }

    [Test]
    public void ArrayTypeTest()
    {
        var function = FunctionDeclarationNode.Create("main", [], new TypeNode("i32[]"), new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        var symbol = TypeSymbol.Array("i32[]");
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("i32[]").WithValue(symbol));
    }

    [Test]
    public void TypeDeclarationTest()
    {
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [],
            [],
            []);
        var tree = new SyntaxTree([type]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("Point").WithValue(TypeSymbol.Type(type)));
    }

    [Test]
    public void FieldDeclarationTest()
    {
        var field1 = new FieldDeclarationNode(AccessModifier.Private, "x", new TypeNode("i32"));
        var field2 = new FieldDeclarationNode(AccessModifier.Private, "y", new TypeNode("i32"));
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [field1, field2],
            [],
            []);
        var tree = new SyntaxTree([type]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(field1.SymbolTable, Is.Not.Null);
        Assert.That(field1.SymbolTable.VariablesInScope, Has.Count.EqualTo(2));
        Assert.That(field1.SymbolTable.VariablesInScope, Contains.Key(field1.Name).WithValue(new VariableSymbol(field1)));
        Assert.That(field1.SymbolTable.VariablesInScope, Contains.Key(field2.Name).WithValue(new VariableSymbol(field2)));
    }

    [Test]
    public void FieldDeclarationAndMethodVariableTest()
    {
        var field1 = new FieldDeclarationNode(AccessModifier.Private, "x", new TypeNode("i32"));
        var field2 = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            LiteralExpressionNode.Number(1));
        var method = new MethodDeclarationNode(
            AccessModifier.Public,
            "test",
            [],
            new TypeNode("void"),
            new BlockStatementNode([field2]));
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [field1],
            [],
            [method]);
        var tree = new SyntaxTree([type]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(field1.SymbolTable, Is.Not.Null);
        Assert.That(field1.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(field1.SymbolTable.VariablesInScope, Contains.Key(field1.Name).WithValue(new VariableSymbol(field1)));

        Assert.That(field2.SymbolTable, Is.Not.Null);
        Assert.That(field2.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(field2.SymbolTable.VariablesInScope, Contains.Key(field1.Name).WithValue(new VariableSymbol(field2)));
    }

    [Test]
    public void CtorDeclarationVariableTest()
    {
        var parameter = new ParameterNode("a", new TypeNode("i32"));
        var ctor = new ConstructorDeclarationNode(AccessModifier.Public, [parameter], new BlockStatementNode());
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [],
            [ctor],
            []);
        var tree = new SyntaxTree([type]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(parameter.SymbolTable, Is.Not.Null);
        Assert.That(parameter.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(parameter.SymbolTable.VariablesInScope, Contains.Key(parameter.Name).WithValue(new VariableSymbol(parameter)));
    }

    [Test]
    public void TypeAliasTest()
    {
        var type = new TypeAliasDeclarationNode(AccessModifier.Public, "MyInt", new TypeNode("i32"));
        var tree = new SyntaxTree([type]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key(type.Name).WithValue(TypeSymbol.Alias(type)));
    }

    [Test]
    public void FunctionTypeAliasTest()
    {
        var type = new FunctionTypeNode([new TypeNode("i32"), new TypeNode("i32")], new TypeNode("i32"));
        var aliasType = new TypeAliasDeclarationNode(AccessModifier.Public, "F", type);
        var tree = new SyntaxTree([aliasType]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(type.Name).WithValue(TypeSymbol.FunctionType(type)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(aliasType.Name).WithValue(TypeSymbol.Alias(aliasType)));
    }
}