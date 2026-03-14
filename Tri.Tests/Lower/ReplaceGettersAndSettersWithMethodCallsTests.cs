using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;

namespace Tri.Tests.Lower;

public class ReplaceGettersAndSettersWithMethodCallsTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();

        var lowering = new Lowering(builtInTypes);
        lowering.Lower(semanticTree, LoweringOptions.Default);

        return semanticTree;
    }

    [Test]
    public void ReplaceReadAccessWithGetterMethodCall()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32;
            }

            public test(p: Point): i32 {
                return p.x;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var point = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        point.AddConstructor(
            new ConstructorMetadata(
                null,
                point,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var count = CreatePropertyMetadata(point, "x", builtInTypes.I32);
        point.AddProperty(count);
        point.AddMethod(count.Getter!);
        point.AddMethod(count.Setter!);

        var backingField = new FieldMetadata(point, $"<>_{count.Name}", count.Type);
        point.AddField(backingField);

        var pParameter = new ParameterMetadata(null, "p", point);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            CreateFunctionType([point], builtInTypes.I32, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            null,
                            null
                        )
                        {
                            Metadata = count,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = point,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "p",
                            new TypeRef(null, "Point") { Metadata = point }
                        )
                        {
                            Metadata = pParameter,
                        }
                    ],
                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ReturnStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(
                                    null,
                                    new MemberAccessExpression(null, "p")
                                    {
                                        Reference = pParameter,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    count.Getter!.Name
                                )
                                {
                                    Reference = count.Getter,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                []
                            )
                        )
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceNestedReadAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; public set; }
            }

            public type Test {
                point: Point;
            }

            public test(t: Test): i32 {
                return t.point.x;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var pointType = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        pointType.AddConstructor(
            new ConstructorMetadata(
                null,
                pointType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var xProperty = CreatePropertyMetadata(pointType,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        pointType.AddProperty(xProperty);
        pointType.AddMethod(xProperty.Getter!);
        pointType.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(pointType, $"<>_{xProperty.Name}", xProperty.Type);
        pointType.AddField(xBackingField);

        var testType = new TypeMetadata(null, "Test")
        {
            Namespace = test1Ns,
        };
        testType.AddConstructor(
            new ConstructorMetadata(
                null,
                testType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var pointProperty = CreatePropertyMetadata(testType, "point", pointType);
        testType.AddProperty(pointProperty);
        testType.AddMethod(pointProperty.Getter!);
        testType.AddMethod(pointProperty.Setter!);

        var pointBackingField = new FieldMetadata(testType, $"<>_{pointProperty.Name}", pointProperty.Type);
        testType.AddField(pointBackingField);

        var tParameter = new ParameterMetadata(null, "t", testType);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [tParameter],
            CreateFunctionType([testType], builtInTypes.I32, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            new PropertyGetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Getter,
                            },
                            new PropertySetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Setter,
                            }
                        )
                        {
                            Metadata = xProperty,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = pointType,
                },
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Test",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "point",
                            new TypeRef(null, "Point") { Metadata = pointType },
                            null,
                            null
                        )
                        {
                            Metadata = pointProperty,
                        }
                    ],
                    [],
                    []
                )
                {
                    Metadata = testType,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "t",
                            new TypeRef(null, "Test") { Metadata = testType }
                        )
                        {
                            Metadata = tParameter,
                        }
                    ],
                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ReturnStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(
                                    null,
                                    new CallExpression(
                                        null,
                                        new MemberAccessExpression(
                                            null,
                                            new MemberAccessExpression(null, "t")
                                            {
                                                Reference = tParameter,
                                                AccessKind = MemberAccessKind.Read,
                                            },
                                            pointProperty.Getter!.Name
                                        )
                                        {
                                            Reference = pointProperty.Getter,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        []
                                    ),
                                    xProperty.Getter!.Name
                                )
                                {
                                    Reference = xProperty.Getter,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                []
                            )
                        )
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceWriteAccessWithSetterMethodCall()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): void {
                p.x = 1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var point = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        point.AddConstructor(
            new ConstructorMetadata(
                null,
                point,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var xProperty = CreatePropertyMetadata(point,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var pParameter = new ParameterMetadata(null, "p", point);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            CreateFunctionType([point], builtInTypes.Void, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            new PropertyGetter(null, AccessModifier.Public, null)
                            {
                                Metadata = xProperty.Getter,
                            },
                            new PropertySetter(null, AccessModifier.Public, null)
                            {
                                Metadata = xProperty.Setter,
                            }
                        )
                        {
                            Metadata = xProperty,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = point,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "p",
                            new TypeRef(null, "Point") { Metadata = point }
                        )
                        {
                            Metadata = pParameter,
                        }
                    ],
                    new TypeRef(null, "void") { Metadata = builtInTypes.Void },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(
                                    null,
                                    new MemberAccessExpression(null, "p")
                                    {
                                        Reference = pParameter,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    xProperty.Setter!.Name
                                )
                                {
                                    Reference = xProperty.Setter,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                [
                                    new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32,
                                    }
                                ]
                            )
                        )
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceReadAndWriteAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): i32 {
                return p.x = 1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var point = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        point.AddConstructor(
            new ConstructorMetadata(
                null,
                point,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var xProperty = CreatePropertyMetadata(point,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var tmpVariable = new VariableMetadata(null, "<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata(null, "p", point);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            CreateFunctionType([point], builtInTypes.I32, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            new PropertyGetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Getter,
                            },
                            new PropertySetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Setter,
                            }
                        )
                        {
                            Metadata = xProperty,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = point,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "p",
                            new TypeRef(null, "Point") { Metadata = point }
                        )
                        {
                            Metadata = pParameter,
                        }
                    ],
                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ReturnStatement(
                            null,
                            new ExpressionBlock([
                                new VariableDeclaration(
                                    null,
                                    tmpVariable.Name,
                                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                                    new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32,
                                    }
                                )
                                {
                                    Metadata = tmpVariable,
                                },
                                new ExpressionStatement(
                                    null,
                                    new CallExpression(
                                        null,
                                        new MemberAccessExpression(
                                            null,
                                            new MemberAccessExpression(null, "p")
                                            {
                                                Reference = pParameter,
                                                AccessKind = MemberAccessKind.Read,
                                            },
                                            xProperty.Setter!.Name
                                        )
                                        {
                                            Reference = xProperty.Setter,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        [
                                            new MemberAccessExpression(null, tmpVariable.Name)
                                            {
                                                Reference = tmpVariable,
                                                AccessKind = MemberAccessKind.Read,
                                            }
                                        ]
                                    )
                                ),
                                new ExpressionStatement(
                                    null,
                                    new MemberAccessExpression(null, tmpVariable.Name)
                                    {
                                        Reference = tmpVariable,
                                        AccessKind = MemberAccessKind.Read,
                                    }
                                )
                            ])
                        )
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceCompoundAssignmentAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): void {
                p.x += 1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var point = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        point.AddConstructor(
            new ConstructorMetadata(
                null,
                point,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var xProperty = CreatePropertyMetadata(point,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var pParameter = new ParameterMetadata(null, "p", point);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            CreateFunctionType([point], builtInTypes.Void, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            new PropertyGetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Getter,
                            },
                            new PropertySetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Setter,
                            }
                        )
                        {
                            Metadata = xProperty,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = point,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "p",
                            new TypeRef(null, "Point") { Metadata = point }
                        )
                        {
                            Metadata = pParameter,
                        }
                    ],
                    new TypeRef(null, "void") { Metadata = builtInTypes.Void },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new CallExpression(
                                null,
                                new MemberAccessExpression(
                                    null,
                                    new MemberAccessExpression(null, "p")
                                    {
                                        Reference = pParameter,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    xProperty.Setter!.Name
                                )
                                {
                                    Reference = xProperty.Setter,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                [
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new CallExpression(
                                            null,
                                            new MemberAccessExpression(
                                                null,
                                                new MemberAccessExpression(null, "p")
                                                {
                                                    Reference = pParameter,
                                                    AccessKind = MemberAccessKind.Read,
                                                },
                                                xProperty.Getter!.Name
                                            )
                                            {
                                                Reference = xProperty.Getter,
                                                AccessKind = MemberAccessKind.Read,
                                            },
                                            []
                                        ),
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32,
                                    }
                                ]
                            )
                        ),
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceReturnCompoundAssignmentAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): i32 {
                return p.x += 1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var point = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        point.AddConstructor(
            new ConstructorMetadata(
                null,
                point,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var xProperty = CreatePropertyMetadata(point,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var tmpVariable = new VariableMetadata(null, "<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata(null, "p", point);
        var testFunction = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            CreateFunctionType([point], builtInTypes.I32, rootNamespace))
        {
            Namespace = rootNamespace,
        };

        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new TypeDeclaration(
                    null,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclaration(
                            null,
                            "x",
                            new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                            new PropertyGetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Getter,
                            },
                            new PropertySetter(
                                null,
                                AccessModifier.Public,
                                null
                            )
                            {
                                Metadata = xProperty.Setter,
                            }
                        )
                        {
                            Metadata = xProperty,
                        },
                    ],
                    [],
                    []
                )
                {
                    Metadata = point,
                },
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(
                            null,
                            "p",
                            new TypeRef(null, "Point") { Metadata = point }
                        )
                        {
                            Metadata = pParameter,
                        }
                    ],
                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new ReturnStatement(
                            null,
                            new ExpressionBlock([
                                new VariableDeclaration(
                                    null,
                                    tmpVariable.Name,
                                    new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new CallExpression(
                                            null,
                                            new MemberAccessExpression(
                                                null,
                                                new MemberAccessExpression(null, "p")
                                                {
                                                    Reference = pParameter,
                                                    AccessKind = MemberAccessKind.Read,
                                                },
                                                xProperty.Getter!.Name
                                            )
                                            {
                                                Reference = xProperty.Getter,
                                                AccessKind = MemberAccessKind.Read,
                                            },
                                            []
                                        ),
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32,
                                    }
                                )
                                {
                                    Metadata = tmpVariable,
                                },
                                new ExpressionStatement(
                                    null,
                                    new CallExpression(
                                        null,
                                        new MemberAccessExpression(
                                            null,
                                            new MemberAccessExpression(null, "p")
                                            {
                                                Reference = pParameter,
                                                AccessKind = MemberAccessKind.Read,
                                            },
                                            xProperty.Setter!.Name
                                        )
                                        {
                                            Reference = xProperty.Setter,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        [
                                            new MemberAccessExpression(null, tmpVariable.Name)
                                            {
                                                Reference = tmpVariable,
                                                AccessKind = MemberAccessKind.Read,
                                            }
                                        ]
                                    )
                                ),
                                new ExpressionStatement(
                                    null,
                                    new MemberAccessExpression(null, tmpVariable.Name)
                                    {
                                        Reference = tmpVariable,
                                        AccessKind = MemberAccessKind.Read,
                                    }
                                )
                            ])
                        )
                    ])
                )
                {
                    Metadata = testFunction,
                },
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}