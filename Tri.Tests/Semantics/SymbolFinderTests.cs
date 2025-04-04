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
        var function = FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));
    }

    [Test]
    public void TwoFunctionsInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var function1 = FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode());
        var function2 = FunctionDeclarationNode.Create("add", [], TypeNode.Create("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function1, function2]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(2));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function1.Name).WithValue(new FunctionSymbol(function1)));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function2.Name).WithValue(new FunctionSymbol(function2)));
    }

    [Test]
    public void SameFunctionInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var function1 = FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode());
        var function2 = FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode());
        var tree = new SyntaxTree([function1, function2]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void FunctionWithParametersInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new FunctionParameterNode("a", TypeNode.Create("i32"));
        var b = new FunctionParameterNode("b", TypeNode.Create("i32"));
        var function = FunctionDeclarationNode.Create(
            "add",
            [a, b],
            TypeNode.Create("void"),
            new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key("add").WithValue(new FunctionSymbol(function)));

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
        var a = new FunctionParameterNode("a", TypeNode.Create("i32"));
        var b = new FunctionParameterNode("a", TypeNode.Create("i32"));
        var function = FunctionDeclarationNode.Create(
            "add",
            [a, b],
            TypeNode.Create("void"),
            new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void FunctionWithVariablesInRootScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1));
        var b = new VariableDeclarationStatementNode("b", TypeNode.Create("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2));
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            TypeNode.Create("void"),
            new BlockStatementNode([a, b]));
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

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
        var a = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1));
        var b = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2));
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            TypeNode.Create("void"),
            new BlockStatementNode([a, b]));
        var tree = new SyntaxTree([function]);

        Assert.Throws<SymbolTableBuilderException>(() => tree.Accept(builder, new SymbolFinderContext()));
    }

    [Test]
    public void IfScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([
                a,
            ])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            TypeNode.Create("void"),
            new BlockStatementNode([ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.VariablesInScope, Contains.Key(a.Name).WithValue(new VariableSymbol(a)));
    }

    [Test]
    public void IfElseScopeTest()
    {
        var builder = new SymbolFinder();
        var a = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var b = new VariableDeclarationStatementNode("b", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([a]),
            new BlockStatementNode([b])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            TypeNode.Create("void"),
            new BlockStatementNode([ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

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
        var a1 = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var a2 = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var a3 = new VariableDeclarationStatementNode("a", TypeNode.Create("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([a2]),
            new BlockStatementNode([a3])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            TypeNode.Create("void"),
            new BlockStatementNode([a1, ifStatement])
        );
        var tree = new SyntaxTree([function]);

        tree.Accept(builder, new SymbolFinderContext());

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Functions, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Functions, Contains.Key(function.Name).WithValue(new FunctionSymbol(function)));

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
        var function = FunctionDeclarationNode.Create("main", [], TypeNode.Array("i32"), new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext());

        var symbol = new TypeSymbol("i32[]", true, null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("i32[]").WithValue(symbol));
    }
}