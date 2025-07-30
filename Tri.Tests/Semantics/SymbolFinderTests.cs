using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class SymbolFinderTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void FunctionInRootScopeTest()
    {
        var tree = Parse("function main(): void { }");
        var function = tree.Find<FunctionDeclarationNode>()!;

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));
    }

    [Test]
    public void TwoFunctionsInRootScopeTest()
    {
        var tree = Parse(
            """
            function main(): void { }
            function add(): void { }
            """);
        var function1 = tree.Find<FunctionDeclarationNode>(x => x.Name == "main")!;
        var function2 = tree.Find<FunctionDeclarationNode>(x => x.Name == "add")!;

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function1.Name).WithValue(new IdSymbol(function1)));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function2.Name).WithValue(new IdSymbol(function2)));
    }

    [Test]
    public void SameFunctionInRootScopeTest()
    {
        var tree = Parse(
            """
            function main(): void { }
            function main(): void { }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'main' function is already defined."));
    }

    [Test]
    public void FunctionWithParametersInRootScopeTest()
    {
        var a = new ParameterNode("a", new TypeNode("i32"));
        var b = new ParameterNode("b", new TypeNode("i32"));
        var function = FunctionDeclarationNode.Create(
            "add",
            [a, b],
            new TypeNode("void"),
            new BlockStatementNode());
        var tree = new SyntaxTree([function]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key("add").WithValue(new IdSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(function.Body.SymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
        Assert.That(function.Body.SymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void FunctionWithSameParametersInRootScopeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [
                    new ParameterNode("a", new TypeNode("i32")),
                    new ParameterNode("a", new TypeNode("i32"))
                ],
                new TypeNode("void"),
                new BlockStatementNode())
        ]);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' parameter is already defined."));
    }

    [Test]
    public void FunctionWithVariablesInRootScopeTest()
    {
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1));
        var b = new VariableDeclarationStatementNode("b", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2));
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([a, b]));
        var tree = new SyntaxTree([function]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(function.Body.SymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
        Assert.That(function.Body.SymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void FunctionWithSameVariablesInRootScopeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 1)),
                    new VariableDeclarationStatementNode("a", new TypeNode("i32"), new LiteralExpressionNode(LiteralExpressionKind.Number, 2))
                ]))
        ]);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' variable is already defined."));
    }

    [Test]
    public void IfScopeTest()
    {
        var a = new VariableDeclarationStatementNode("a", new TypeNode("i32"), LiteralExpressionNode.Number(1));
        var ifStatement = new IfStatementNode(
            LiteralExpressionNode.True(),
            new BlockStatementNode([a])
        );
        var function = FunctionDeclarationNode.Create(
            "main",
            [],
            new TypeNode("void"),
            new BlockStatementNode([ifStatement])
        );
        var tree = new SyntaxTree([function]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
    }

    [Test]
    public void IfElseScopeTest()
    {
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));

        Assert.That(ifStatement.Else, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Else.SymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void SameVariableInMultipleScopesTest()
    {
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        Assert.That(function.Body, Is.Not.Null);
        Assert.That(function.Body.SymbolTable, Is.Not.Null);
        Assert.That(function.Body.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(function.Body.SymbolTable.Ids, Contains.Key(a1.Name).WithValue(new IdSymbol(a1)));

        Assert.That(ifStatement.Then.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Then.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Then.SymbolTable.Ids, Contains.Key(a2.Name).WithValue(new IdSymbol(a2)));

        Assert.That(ifStatement.Else, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable, Is.Not.Null);
        Assert.That(ifStatement.Else.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(ifStatement.Else.SymbolTable.Ids, Contains.Key(a3.Name).WithValue(new IdSymbol(a3)));
    }

    [Test]
    public void ArrayTypeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new ArrayTypeNode(new TypeNode("i32")),
                new BlockStatementNode()
            )
        ]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var arrayTypeNode = tree.Find<ArrayTypeNode>();
        Assert.That(arrayTypeNode, Is.Not.Null);

        var symbol = TypeSymbol.Array(arrayTypeNode);
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
            [],
            [],
            []);
        var tree = new SyntaxTree([type]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("Point").WithValue(TypeSymbol.Type(type)));
    }

    [Test]
    public void TypeDeclarationDuplicateTest()
    {
        var type1 = new TypeDeclarationNode(AccessModifier.Public, "Point", [], [], [], [], []);
        var type2 = new TypeDeclarationNode(AccessModifier.Public, "Point", [], [], [], [], []);
        var tree = new SyntaxTree([type1, type2]);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'Point' type is already defined."));
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
            [],
            [],
            [ctor],
            []);
        var tree = new SyntaxTree([type]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(ctor.Body.SymbolTable, Is.Not.Null);
        Assert.That(ctor.Body.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(ctor.Body.SymbolTable.Ids, Contains.Key("this").WithValue(new IdSymbol("this", type)));
        Assert.That(ctor.Body.SymbolTable.Ids, Contains.Key(parameter.Name).WithValue(new IdSymbol(parameter)));
    }

    [Test]
    public void MethodDeclarationVariableTest()
    {
        var parameter = new ParameterNode("a", new TypeNode("i32"));
        var method = new MethodDeclarationNode(
            AccessModifier.Public,
            false,
            "test",
            [parameter],
            new TypeNode("void"),
            new BlockStatementNode());
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [],
            [],
            [],
            [],
            [method]);
        var tree = new SyntaxTree([type]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(method.Body.SymbolTable, Is.Not.Null);
        Assert.That(method.Body.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(method.Body.SymbolTable.Ids, Contains.Key("this").WithValue(new IdSymbol("this", type)));
        Assert.That(method.Body.SymbolTable.Ids, Contains.Key(parameter.Name).WithValue(new IdSymbol(parameter)));
    }

    [Test]
    public void TypeAliasTest()
    {
        var type = new TypeAliasDeclarationNode(AccessModifier.Public, "MyInt", [], new TypeNode("i32"));
        var tree = new SyntaxTree([type]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key(type.Name).WithValue(TypeSymbol.Alias(type)));
    }

    [Test]
    public void TypeAliasDuplicateTest()
    {
        var type1 = new TypeAliasDeclarationNode(AccessModifier.Public, "MyInt", [], new TypeNode("i32"));
        var type2 = new TypeAliasDeclarationNode(AccessModifier.Public, "MyInt", [], new TypeNode("i32"));
        var tree = new SyntaxTree([type1, type2]);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'MyInt' type is already defined."));
    }

    [Test]
    public void FunctionTypeAliasTest()
    {
        var type = new FunctionTypeNode([new TypeNode("i32"), new TypeNode("i32")], new TypeNode("i32"));
        var aliasType = new TypeAliasDeclarationNode(AccessModifier.Public, "F", [], type);
        var tree = new SyntaxTree([aliasType]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(type.Name).WithValue(TypeSymbol.FunctionType(type)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(aliasType.Name).WithValue(TypeSymbol.Alias(aliasType)));
    }

    [Test]
    public void TypeIdsInSymbolTableTest()
    {
        var type = new TypeDeclarationNode(
            AccessModifier.Public,
            "Point",
            [],
            [],
            [
                new PropertyDeclarationNode("x", new TypeNode("i32")),
                new PropertyDeclarationNode("y", new TypeNode("i32")),
            ],
            [],
            [
                new MethodDeclarationNode(
                    AccessModifier.Public,
                    false,
                    "toString",
                    [],
                    new TypeNode("string"),
                    new BlockStatementNode()),
                new MethodDeclarationNode(
                    AccessModifier.Public,
                    false,
                    "distance",
                    [new ParameterNode("other", new TypeNode("Point"))],
                    new TypeNode("f32"),
                    new BlockStatementNode()),
            ]);
        var tree = new SyntaxTree([type]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key(type.Name).WithValue(TypeSymbol.Type(type)));

        Assert.That(type.SymbolTable, Is.Not.Null);
        Assert.That(type.SymbolTable.Ids, Has.Count.EqualTo(4));
        Assert.That(type.SymbolTable.Ids, Contains.Key("x").WithValue(new IdSymbol(type.Properties[0])));
        Assert.That(type.SymbolTable.Ids, Contains.Key("y").WithValue(new IdSymbol(type.Properties[1])));
        Assert.That(type.SymbolTable.Ids, Contains.Key("toString").WithValue(new IdSymbol(type.Methods[0])));
        Assert.That(type.SymbolTable.Ids, Contains.Key("distance").WithValue(new IdSymbol(type.Methods[1])));
    }

    [Test]
    public void InterfaceIdsInSymbolTableTest()
    {
        var @interface = new InterfaceNode(
            [
                new InterfacePropertyNode("x", new TypeNode("i32"), null, null),
                new InterfacePropertyNode("y", new TypeNode("i32"), null, null),
            ],
            [
                new InterfaceMethodNode("toString", [], new TypeNode("string")),
                new InterfaceMethodNode("distance", [new TypeNode("Point")], new TypeNode("f32")),
            ]
        );
        var alias = new TypeAliasDeclarationNode(AccessModifier.Public, "Point", [], @interface);
        var tree = new SyntaxTree([alias]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(tree.SymbolTable.Types, Contains.Key(@interface.Name).WithValue(TypeSymbol.Interface(@interface)));
        Assert.That(tree.SymbolTable.Types, Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));

        Assert.That(@interface.SymbolTable, Is.Not.Null);
        Assert.That(@interface.SymbolTable.Ids, Has.Count.EqualTo(4));
        Assert.That(@interface.SymbolTable.Ids, Contains.Key("x").WithValue(new IdSymbol(@interface.Properties[0])));
        Assert.That(@interface.SymbolTable.Ids, Contains.Key("y").WithValue(new IdSymbol(@interface.Properties[1])));
        Assert.That(@interface.SymbolTable.Ids, Contains.Key("toString").WithValue(new IdSymbol(@interface.Methods[0])));
        Assert.That(@interface.SymbolTable.Ids, Contains.Key("distance").WithValue(new IdSymbol(@interface.Methods[1])));
    }

    [Test]
    public void InlineFunctionTypeInInterfaceTest()
    {
        var @interface = new InterfaceNode(
            [
                new InterfacePropertyNode("x", new FunctionTypeNode([], new TypeNode("void")), null, null),
            ],
            []
        );
        var alias = new TypeAliasDeclarationNode(AccessModifier.Public, "Point", [], @interface);
        var tree = new SyntaxTree([alias]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(tree.SymbolTable.Types, Contains.Key(@interface.Name).WithValue(TypeSymbol.Interface(@interface)));
        Assert.That(tree.SymbolTable.Types, Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));
    }

    [Test]
    public void DiscriminatedUnionTest()
    {
        var discriminatedUnionNode = new DiscriminatedUnionNode([
            new InterfaceNode([], []),
            new TypeNode("i32"),
            new FunctionTypeNode([], new TypeNode("void")),
        ]);
        var alias = new TypeAliasDeclarationNode(
            AccessModifier.Public,
            "T",
            [],
            discriminatedUnionNode);
        var tree = new SyntaxTree([alias]);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(4));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(discriminatedUnionNode.Name)
                .WithValue(TypeSymbol.DiscriminatedUnion(discriminatedUnionNode)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));
    }

    [Test]
    public void TupleTypeTest()
    {
        var tree = Parse("public type T = (i32, bool);");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var aliasNode = tree.Find<TypeAliasDeclarationNode>();
        var tupleNode = tree.Find<TupleTypeNode>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(tupleNode, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("(i32, bool)").WithValue(TypeSymbol.Tuple(tupleNode)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("T").WithValue(TypeSymbol.Alias(aliasNode)));
    }

    [Test]
    public void NestedTupleTypeTest()
    {
        var tree = Parse("public type T = ((i32, i32), bool);");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var aliasNode = tree.Find<TypeAliasDeclarationNode>();
        var tupleNode = tree.Find<TupleTypeNode>(x => x.Name == "((i32, i32), bool)");
        var nestedTupleNode = tree.Find<TupleTypeNode>(x => x.Name == "(i32, i32)");

        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(tupleNode, Is.Not.Null);
        Assert.That(nestedTupleNode, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("(i32, i32)").WithValue(TypeSymbol.Tuple(nestedTupleNode)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("((i32, i32), bool)").WithValue(TypeSymbol.Tuple(tupleNode)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("T").WithValue(TypeSymbol.Alias(aliasNode)));
    }

    [Test]
    public void FieldInGetterTest()
    {
        var tree = Parse(
            """
            public type Test {
                x: i32 {
                    private get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var property = tree.Find<PropertyDeclarationNode>();
        var getter = tree.Find<PropertyGetterNode>();
        Assert.That(getter, Is.Not.Null);
        Assert.That(getter.Body, Is.Not.Null);
        Assert.That(getter.Body.SymbolTable, Is.Not.Null);
        Assert.That(getter.Body.SymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(getter.Body.SymbolTable.Ids, Contains.Key("field").WithValue(new IdSymbol("field", property)));

        var setter = tree.Find<PropertySetterNode>();
        Assert.That(setter, Is.Not.Null);
        Assert.That(setter.Body, Is.Not.Null);
        Assert.That(setter.Body.SymbolTable, Is.Not.Null);
        Assert.That(setter.Body.SymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(setter.Body.SymbolTable.Ids, Contains.Key("field").WithValue(new IdSymbol("field", property)));
        Assert.That(setter.Body.SymbolTable.Ids, Contains.Key("value").WithValue(new IdSymbol("value", property)));
    }

    [Test]
    public void GenericTypeTest()
    {
        var tree = Parse("public type List<T> { }");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = tree.Find<TypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.SymbolTable, Is.Not.Null);
        Assert.That(type.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(type.SymbolTable.Types, Contains.Key("List<>").WithValue(TypeSymbol.OpenGenericType(type)));
    }

    [Test]
    public void GenericTypeWithMultipleTypeArgumentsTest()
    {
        var tree = Parse("public type Test<T1, T2> { }");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = tree.Find<TypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.SymbolTable, Is.Not.Null);
        Assert.That(type.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(type.SymbolTable.Types, Contains.Key("Test<,>").WithValue(TypeSymbol.OpenGenericType(type)));
    }

    [Test]
    public void SymbolForClosedGenericTypeTest()
    {
        var tree = Parse(
            """
            public type List<T> { }

            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var alias = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(alias, Is.Not.Null);

        var closedGeneric = tree.Find<GenericTypeNode>();
        Assert.That(closedGeneric, Is.Not.Null);

        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("Test").WithValue(TypeSymbol.Alias(alias)));
        Assert.That(
            tree.SymbolTable.Types,
            Contains.Key("List<i32>").WithValue(TypeSymbol.GenericType(closedGeneric)));
    }
}