using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class ReplaceIfDirectivesTests
{
    [Test]
    public void ReplaceIfDirectiveWithThenDeclarationsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """);
        var (tree, diagnostics, _) = Lower(file, ["D1"]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var type1Metadata = new TypeMetadata(null, "Type1")
        {
            Namespace = test1Ns,
        };
        type1Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type1Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var type3Metadata = new TypeMetadata(null, "Type3")
        {
            Namespace = test1Ns,
        };
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(null, AccessModifier.Public, "Type1", [], [], [], [], [])
                {
                    Metadata = type1Metadata,
                },
                new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
                {
                    Metadata = type3Metadata,
                },
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseDeclarationsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """);
        var (tree, diagnostics, _) = Lower(file, []);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var type2Metadata = new TypeMetadata(null, "Type2")
        {
            Namespace = test1Ns,
        };
        type2Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type2Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var type3Metadata = new TypeMetadata(null, "Type3")
        {
            Namespace = test1Ns,
        };
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(null, AccessModifier.Public, "Type2", [], [], [], [], [])
                {
                    Metadata = type2Metadata,
                },
                new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
                {
                    Metadata = type3Metadata,
                },
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void RemoveIfDirectiveDeclarationsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #endif

            public type Type3 { }
            """);
        var (tree, diagnostics, _) = Lower(file, []);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var typeMetadata = new TypeMetadata(null, "Type3")
        {
            Namespace = test1Ns,
        };
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
                {
                    Metadata = typeMetadata,
                },
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceIfDirectiveWithThenStatementsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """);
        var (tree, diagnostics, _) = Lower(file, ["D1"]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "callback",
                            new FunctionType(null, [], new TypeRef(null, null, ["void"]) { Metadata = builtInTypes.Void })
                            {
                                Metadata = CreateFunctionType([], builtInTypes.Void, rootNamespace),
                            }
                        )
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(null, "callback")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                []
                            )
                        ),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        )
                    ])
                )
                {
                    Metadata = new FunctionMetadata(
                        null,
                        AccessModifierMetadata.Public,
                        "test",
                        [parameterMetadata],
                        CreateFunctionType(
                            [CreateFunctionType([], builtInTypes.Void, rootNamespace)],
                            builtInTypes.I32,
                            rootNamespace))
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseStatementsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """);
        var (tree, diagnostics, _) = Lower(file, []);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "callback",
                            new FunctionType(null, [], new TypeRef(null, null, ["void"]) { Metadata = builtInTypes.Void })
                            {
                                Metadata = CreateFunctionType([], builtInTypes.Void, rootNamespace)
                            }
                        )
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(null, "callback")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                []
                            )
                        ),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 2)
                            {
                                ReturnTypeMetadata = builtInTypes.I32
                            }
                        )
                    ])
                )
                {
                    Metadata = new FunctionMetadata(
                        null,
                        AccessModifierMetadata.Public,
                        "test",
                        [parameterMetadata],
                        CreateFunctionType(
                            [CreateFunctionType([], builtInTypes.Void, rootNamespace)],
                            builtInTypes.I32,
                            rootNamespace))
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void RemoveIfDirectiveWithStatementsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #endif

                return 2;
            }
            """);
        var (tree, diagnostics, _) = Lower(file, []);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "callback",
                            new FunctionType(null, [], new TypeRef(null, null, ["void"]) { Metadata = builtInTypes.Void })
                            {
                                Metadata = CreateFunctionType([], builtInTypes.Void, rootNamespace),
                            }
                        )
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(null, "callback")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                []
                            )
                        ),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 2)
                            {
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        )
                    ])
                )
                {
                    Metadata = new FunctionMetadata(
                        null,
                        AccessModifierMetadata.Public,
                        "test",
                        [parameterMetadata],
                        CreateFunctionType(
                            [CreateFunctionType([], builtInTypes.Void, rootNamespace)],
                            builtInTypes.I32,
                            rootNamespace))
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}