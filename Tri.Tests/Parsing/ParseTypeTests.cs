using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);
        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void ParseTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "MyType",
                    [],
                    new TypeRefNode(default, ["i32"])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeAliasMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type = i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "<>_0",
                    [],
                    new TypeRefNode(default, ["i32"])
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0005ExpectedTypeName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(30, 3, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = ;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "MyType",
                    [],
                    new FakeTypeNode(
                        default,
                        "<>_0"
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = i32");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "MyType",
                    [],
                    new TypeRefNode(default, ["i32"])
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(42, 3, 25).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = () => void;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [],
                        new TypeRefNode(default, ["void"])
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeWithParametersTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [new TypeRefNode(default, ["i32"]), new TypeRefNode(default, ["i32"])],
                        new TypeRefNode(default, ["i32"])
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type = (i32, i32) => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "<>_0",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new TypeRefNode(default, ["i32"])
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0005ExpectedTypeName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(30, 3, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeMissingEqualTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F (i32, i32) => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    [],
                    [],
                    [],
                    [])
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0014ExpectedTypeMember,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(32, 3, 15), new SourcePosition(50, 3, 33))),
                "Expected a type member (a property, a method or a constructor).")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = i32, i32) => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new TypeRefNode(default, ["i32"])
                ),
                new FakeDeclarationNode(default)
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(37, 3, 20), new SourcePosition(51, 3, 34))),
                "Expected a type or a function.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32 => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new TypeRefNode(default, ["i32"])
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(44, 3, 27).ToSpan()),
                "Expected ')'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeCommaTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32 i32) => i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new TypeRefNode(default, ["i32"])
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
                "Expected ','.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeArrowTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) i32;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new TupleTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"]),
                        ]
                    )
                ),
                new FakeDeclarationNode(default),
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(45, 3, 28).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(45, 3, 28), new SourcePosition(49, 3, 32))),
                "Expected a type or a function.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => ;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new FakeTypeNode(
                            default,
                            "<>_0"
                        )
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0003ExpectedType,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(48, 3, 31).ToSpan()),
                "Expected a type.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeSemiColonTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => i32");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "F",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new TypeRefNode(default, ["i32"])
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(51, 3, 34).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseArrayOfFunctionTypesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (() => i32)[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new FunctionTypeNode(
                            default,
                            [],
                            new TypeRefNode(default, ["i32"]))))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseFunctionTypeInParameterTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(callback: (i32, i32) => void): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "callback",
                            new FunctionTypeNode(
                                default,
                                [
                                    new TypeRefNode(default, ["i32"]),
                                    new TypeRefNode(default, ["i32"])
                                ],
                                new TypeRefNode(default, ["void"])
                            )
                        )
                    ],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(default, [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeInReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): (i32, i32) => void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new FunctionTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ],
                        new TypeRefNode(default, ["void"])
                    ),
                    new BlockStatementNode(default, [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeInVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: (i32, i32) => void = 0;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new FunctionTypeNode(
                                default,
                                [new TypeRefNode(default, ["i32"]), new TypeRefNode(default, ["i32"])],
                                new TypeRefNode(default, ["void"])
                            ),
                            LiteralExpressionNode.Integer(default, 0)
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(default, "x", new TypeRefNode(default, ["i32"]), null, null),
                            new InterfacePropertyNode(default, "y", new TypeRefNode(default, ["i32"]), null, null)
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [new TypeRefNode(default, ["Point"])],
                                new TypeRefNode(default, ["f32"])
                            )
                        ]
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithGettersSettersTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public get; public set; }
                y: i32 { private get; private set; }

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                AccessModifier.Public,
                                AccessModifier.Public
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                AccessModifier.Private,
                                AccessModifier.Private
                            )
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [new TypeRefNode(default, ["Point"])],
                                new TypeRefNode(default, ["f32"])
                            )
                        ]
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithGetOnlyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public get; }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                AccessModifier.Public,
                                null
                            ),
                        ],
                        []
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithSetOnlyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public set; }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                AccessModifier.Public
                            ),
                        ],
                        []
                    )
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new TypeRefNode(default, ["f32"])
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(90, 7, 26).ToSpan()),
            "Expected '}'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertyTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: ;
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new FakeTypeNode(
                                    default,
                                    "<>_0"
                                ),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new TypeRefNode(default, ["f32"])
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(47, 4, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertySemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new TypeRefNode(default, ["f32"])
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(55, 5, 5).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(55, 5, 5).ToSpan()),
                "Expected '}'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): ;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new FakeTypeNode(
                                    default,
                                    "<>_0"
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(86, 7, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point) f64;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new TypeRefNode(default, ["f64"])
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(85, 7, 21).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f64
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, ["i32"]),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(default, ["Point"])
                                ],
                                new TypeRefNode(default, ["f64"])
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(90, 8, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseArrayOfInterfacesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = { x: i32; }[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new InterfaceNode(
                            default,
                            [
                                new InterfacePropertyNode(
                                    default,
                                    "x",
                                    new TypeRefNode(default, ["i32"]),
                                    null,
                                    null)
                            ],
                            [])))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = { } | i32 | () => void;");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new DiscriminatedUnionNode([
                        new InterfaceNode(default, [], []),
                        new TypeRefNode(default, ["i32"]),
                        new FunctionTypeNode(default, [], new TypeRefNode(default, ["void"])),
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayOfDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (i32 | null)[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new DiscriminatedUnionNode(
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["null"])
                        ])))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayInDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = null | i32[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new DiscriminatedUnionNode(
                    [
                        new TypeRefNode(default, ["null"]),
                        new ArrayTypeNode(
                            default,
                            new TypeRefNode(default, ["i32"]))
                    ]))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseNullTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: null = null;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, ["null"]),
                            new NullExpressionNode(default)
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (i32, i32);");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new TupleTypeNode(default, [new TypeRefNode(default, ["i32"]), new TypeRefNode(default, ["i32"])])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = ((i32, i32), i32);");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new TupleTypeNode(default, [
                        new TupleTypeNode(default, [new TypeRefNode(default, ["i32"]), new TypeRefNode(default, ["i32"])]),
                        new TypeRefNode(default, ["i32"])
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (bool | i32, () => void);");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new TupleTypeNode(
                        default,
                        [
                            new DiscriminatedUnionNode([
                                new TypeRefNode(default, ["bool"]),
                                new TypeRefNode(default, ["i32"])
                            ]),
                            new FunctionTypeNode(
                                default,
                                [],
                                new TypeRefNode(default, ["void"])
                            )
                        ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleInDuTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (bool, f64);");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new DiscriminatedUnionNode([
                        new TypeRefNode(default, ["i32"]),
                        new TupleTypeNode(default, [new TypeRefNode(default, ["bool"]), new TypeRefNode(default, ["f64"])])
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeInParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (i32);");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new TypeRefNode(default, ["i32"])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeMissingTest()
    {
        const string code = "namespace Test1;\n\npublic type T = (i32";

        var (tree, diagnostics) = Parse(code);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new TypeRefNode(default, ["i32"])
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
                "Expected ')'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
                "Expected ';'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void FunctionWithTupleTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic main(): (i32, i32) { }");
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TupleTypeNode(
                        default,
                        [
                            new TypeRefNode(default, ["i32"]),
                            new TypeRefNode(default, ["i32"])
                        ]
                    ),
                    new BlockStatementNode(default, [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDuTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (() => i32 | null);");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new DiscriminatedUnionNode([
                        new TypeRefNode(default, ["i32"]),
                        new FunctionTypeNode(
                            default,
                            [],
                            new DiscriminatedUnionNode([
                                new TypeRefNode(default, ["i32"]),
                                new TypeRefNode(default, ["null"]),
                            ])
                        )
                    ])
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDu2Test()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (() => i32) | null;");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new DiscriminatedUnionNode([
                        new TypeRefNode(default, ["i32"]),
                        new FunctionTypeNode(
                            default,
                            [],
                            new TypeRefNode(default, ["i32"])
                        ),
                        new TypeRefNode(default, ["null"]),
                    ])
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayOfTypesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = i32[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new TypeRefNode(default, ["i32"])))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayOfTuplesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (i32, null)[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new TupleTypeNode(
                            default,
                            [
                                new TypeRefNode(default, ["i32"]),
                                new TypeRefNode(default, ["null"])
                            ])))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayOfArraysTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = i32[][];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new ArrayTypeNode(
                        default,
                        new ArrayTypeNode(
                            default,
                            new TypeRefNode(default, ["i32"]))))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayInReturnOfFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = () => i32[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "T",
                    [],
                    new FunctionTypeNode(
                        default,
                        [],
                        new ArrayTypeNode(
                            default,
                            new TypeRefNode(default, ["i32"]))))
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayOfGenericsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> { }

            public type Test = List<i32>[];
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "List",
                    [new TypeRefNode(default, ["T"])],
                    [],
                    [],
                    [],
                    []),
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Test",
                    [],
                    new ArrayTypeNode(
                        default,
                        new GenericApplicationNode(
                            default,
                            new TypeRefNode(default, ["List"]),
                            [new TypeRefNode(default, ["i32"])]))),
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayInGenericTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> { }

            public type Test = List<i32[]>;
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "List",
                    [new TypeRefNode(default, ["T"])],
                    [],
                    [],
                    [],
                    []),
                new AliasDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Test",
                    [],
                    new GenericApplicationNode(
                        default,
                        new TypeRefNode(default, ["List"]),
                        [
                            new ArrayTypeNode(
                                default,
                                new TypeRefNode(default, ["i32"]))
                        ])),
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}