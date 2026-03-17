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
    public void ParseTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "<>_0",
                    [],
                    [],
                    [],
                    [],
                    []
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
    public void ParseTypeMissingOpenBraceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(36, 3, 19).ToSpan()),
            "Expected '{'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point {");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertiesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32;
                y: i32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32")
                        ),
                        new PropertyDeclarationNode(
                            default,
                            "y",
                            new TypeRefNode(default, "i32")
                        ),
                    ],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePropertiesWithBlocksTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get {
                        return field;
                    }
                    private set {
                        field = value;
                    }
                }
                y: i32 {
                    private get{
                        return field;
                    }
                    private set {
                        field = value;
                    }
                }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new PropertyGetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ReturnStatementNode(
                                            default,
                                            new MemberAccessExpressionNode(
                                                default,
                                                "field"
                                            )
                                        )
                                    ])
                            ),
                            new PropertySetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ExpressionStatementNode(
                                            default,
                                            new BinaryExpressionNode(
                                                default,
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "field"
                                                ),
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "value"
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                        ),
                        new PropertyDeclarationNode(
                            default,
                            "y",
                            new TypeRefNode(default, "i32"),
                            new PropertyGetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ReturnStatementNode(
                                            default,
                                            new MemberAccessExpressionNode(
                                                default,
                                                "field"
                                            )
                                        )
                                    ]
                                )
                            ),
                            new PropertySetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ExpressionStatementNode(
                                            default,
                                            new BinaryExpressionNode(
                                                default,
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "field"
                                                ),
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "value"
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                        ),
                    ],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptyGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get;
                    private set {
                        field = value;
                    }
                }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new PropertyGetterNode(
                                default,
                                AccessModifier.Private,
                                null
                            ),
                            new PropertySetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ExpressionStatementNode(
                                            default,
                                            new BinaryExpressionNode(
                                                default,
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "field"
                                                ),
                                                new MemberAccessExpressionNode(
                                                    default,
                                                    "value"
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                        ),
                    ],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptySetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get {
                        return field;
                    }
                    private set;
                }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new PropertyGetterNode(
                                default,
                                AccessModifier.Private,
                                new BlockStatementNode(default, [
                                    new ReturnStatementNode(
                                        default,
                                        new MemberAccessExpressionNode(default, "field")
                                    )
                                ])
                            ),
                            new PropertySetterNode(default, AccessModifier.Private, null)
                        ),
                    ],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePropertyMissingNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                : i32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(42, 4, 5), new SourcePosition(49, 5, 1))),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x i32;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32")
                        ),
                    ],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(44, 4, 7).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: ;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new FakeTypeNode(
                                default,
                                "<>_0"
                            )
                        ),
                    ],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(45, 4, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [
                        new PropertyDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32")
                        ),
                    ],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 5, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string { }

                public distance(other: Point): f32 { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(default, [])
                        ),
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "distance",
                            [new ParameterNode(default, "other", new TypeRefNode(default, "Point"))],
                            new TypeRefNode(default, "f32"),
                            new BlockStatementNode(default, [])
                        )
                    ]
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMethodMissingNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public (): string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "<>_0",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(default)
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0007ExpectedMethodName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 4, 12).ToSpan()),
            "Expected a method name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString): string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(49, 2, 30), new SourcePosition(52, 2, 33))
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(57, 4, 20).ToSpan()),
            "Expected '('.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(: string { return; }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(
                                default,
                                [
                                    new ReturnStatementNode(default)
                                ]
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(58, 4, 21).ToSpan()),
                "Expected ')'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMethodMissingColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString() string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(49, 2, 30), new SourcePosition(52, 2, 33))
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(60, 4, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): { var x: i32 = 1; }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new InterfaceNode(
                                default,
                                [],
                                []
                            ),
                            new BlockStatementNode(
                                default,
                                [
                                    new VariableDeclarationNode(
                                        default,
                                        "x",
                                        new TypeRefNode(default, "i32"),
                                        new LiteralExpressionNode(
                                            default,
                                            LiteralExpressionKind.Integer,
                                            1
                                        )
                                    )
                                ]
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(63, 4, 26).ToSpan()),
                "Expected '}'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(63, 4, 26).ToSpan()),
                "Expected '{'."),
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMethodMissingOpenBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(default)
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(68, 4, 31).ToSpan()),
            "Expected '{'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string {
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(50, 2, 31), new SourcePosition(53, 3, 2))
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(71, 5, 2).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCommaTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a: i32 b: i32): string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [
                                new ParameterNode(
                                    default,
                                    "a",
                                    new TypeRefNode(default, "i32")
                                ),
                                new ParameterNode(
                                    default,
                                    "b",
                                    new TypeRefNode(default, "i32")
                                ),
                            ],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(default)
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(65, 4, 28), new SourcePosition(65, 4, 28))),
            "Expected ','.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a i32): string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [
                                new ParameterNode(
                                    default,
                                    "a",
                                    new TypeRefNode(default, "i32")
                                ),
                            ],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(default)
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(60, 4, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a: ): string { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            false,
                            "toString",
                            [
                                new ParameterNode(
                                    default,
                                    "a",
                                    new FakeTypeNode(
                                        default,
                                        "<>_0"
                                    )
                                ),
                            ],
                            new TypeRefNode(default, "string"),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(53, 2, 34), new SourcePosition(56, 2, 37))
                            )
                        )
                    ]
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(61, 4, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) { }
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [
                        new ConstructorDeclarationNode(
                            default,
                            AccessModifier.Public,
                            [
                                new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                                new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                            ],
                            new BlockStatementNode(default, [])
                        )
                    ],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeWithInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1, Interface2 { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [new TypeRefNode(default, "Interface1"), new TypeRefNode(default, "Interface2")],
                    [],
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeWithMissingInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0006ExpectedInterface,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
            "Expected an interface.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingCommaInInterfacesTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1 Interface2 { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [
                        new TypeRefNode(
                            default,
                            "Interface1"
                        ),
                        new TypeRefNode(
                            default,
                            "Interface2"
                        )
                    ],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 3, 32).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingSecondInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1, { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Point",
                    [],
                    [
                        new TypeRefNode(
                            default,
                            "Interface1"
                        ),
                    ],
                    [],
                    [],
                    []
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0006ExpectedInterface,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(50, 3, 33).ToSpan()),
            "Expected an interface.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
                    new TypeRefNode(default, "i32")
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
                    new TypeRefNode(default, "i32")
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
                    new TypeRefNode(default, "i32")
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
                        new TypeRefNode(default, "void")
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
                        [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")],
                        new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ],
                        new TypeRefNode(default, "i32")
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
                    new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ],
                        new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ],
                        new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32"),
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ],
                        new TypeRefNode(default, "i32")
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
                                    new TypeRefNode(default, "i32"),
                                    new TypeRefNode(default, "i32")
                                ],
                                new TypeRefNode(default, "void")
                            )
                        )
                    ],
                    new TypeRefNode(default, "void"),
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ],
                        new TypeRefNode(
                            default,
                            "void"
                        )
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new FunctionTypeNode(
                                default,
                                [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")],
                                new TypeRefNode(default, "void")
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
                            new InterfacePropertyNode(default, "x", new TypeRefNode(default, "i32"), null, null),
                            new InterfacePropertyNode(default, "y", new TypeRefNode(default, "i32"), null, null)
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [new TypeRefNode(default, "Point")],
                                new TypeRefNode(default, "f32")
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
                                new TypeRefNode(default, "i32"),
                                AccessModifier.Public,
                                AccessModifier.Public
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                AccessModifier.Private,
                                AccessModifier.Private
                            )
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [new TypeRefNode(default, "Point")],
                                new TypeRefNode(default, "f32")
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
                                new TypeRefNode(default, "i32"),
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
                                new TypeRefNode(default, "i32"),
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
                                ],
                                new TypeRefNode(
                                    default,
                                    "f32"
                                )
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
                                ],
                                new TypeRefNode(
                                    default,
                                    "f32"
                                )
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
                                ],
                                new TypeRefNode(
                                    default,
                                    "f32"
                                )
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
                                ],
                                new TypeRefNode(
                                    default,
                                    "f64"
                                )
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
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                            new InterfacePropertyNode(
                                default,
                                "y",
                                new TypeRefNode(default, "i32"),
                                null,
                                null
                            ),
                        ],
                        [
                            new InterfaceMethodNode(
                                default,
                                "distance",
                                [
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    )
                                ],
                                new TypeRefNode(
                                    default,
                                    "f64"
                                )
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
    public void ParseNewOperatorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point();
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "p",
                            new TypeRefNode(default, "Point"),
                            new NewObjectExpressionNode(default, new TypeRefNode(default, "Point"), [])
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorWithParametersTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, 2);
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "p",
                            new TypeRefNode(default, "Point"),
                            new NewObjectExpressionNode(
                                default,
                                new TypeRefNode(default, "Point"),
                                [
                                    LiteralExpressionNode.Integer(default, 1),
                                    LiteralExpressionNode.Integer(default, 2)
                                ]
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new ();
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new FakeTypeNode(
                                        default,
                                        "<>_0"
                                    ),
                                    []
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(63, 4, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingArgumentTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, );
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    ),
                                    [
                                        LiteralExpressionNode.Integer(
                                            default,
                                            1
                                        ),
                                        new FakeExpressionNode(
                                            new SourcePosition(54, 2, 33).ToSpan()
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(72, 4, 33).ToSpan()),
            "Expected an expression.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    ),
                                    []
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(69, 4, 30).ToSpan()),
            "Expected ')'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
                        new TypeRefNode(default, "i32"),
                        new FunctionTypeNode(default, [], new TypeRefNode(default, "void")),
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNullTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: i32 | null = null;
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new DiscriminatedUnionNode([
                                new TypeRefNode(default, "i32"),
                                new TypeRefNode(default, "null")
                            ]),
                            new NullExpressionNode(default)
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeTest()
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
                    new TupleTypeNode(default, [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void NestedTupleTypeTest()
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
                        new TupleTypeNode(default, [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")]),
                        new TypeRefNode(default, "i32")
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeWithDuTest()
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
                                new TypeRefNode(default, "bool"),
                                new TypeRefNode(default, "i32")
                            ]),
                            new FunctionTypeNode(
                                default,
                                [],
                                new TypeRefNode(default, "void")
                            )
                        ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeWithSingleTypeTest()
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
                    new TypeRefNode(default, "i32")
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
                    new TypeRefNode(default, "i32")
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
                            new TypeRefNode(default, "i32"),
                            new TypeRefNode(default, "i32")
                        ]
                    ),
                    new BlockStatementNode(default, [])
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
                        new TypeRefNode(default, "i32"),
                        new TupleTypeNode(default, [new TypeRefNode(default, "bool"), new TypeRefNode(default, "f64")])
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (bool, i32 | f64);");
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
                        new TypeRefNode(default, "bool"),
                        new DiscriminatedUnionNode([new TypeRefNode(default, "i32"), new TypeRefNode(default, "f64")])
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseStaticMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                public static test(): void { }
            }
            """);
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Test",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            true,
                            "test",
                            [],
                            new TypeRefNode(default, "void"),
                            new BlockStatementNode(default)
                        )
                    ]
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallStaticMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new TypeDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "Test",
                    [],
                    [],
                    [],
                    [],
                    [
                        new MethodDeclarationNode(
                            default,
                            AccessModifier.Public,
                            true,
                            "test",
                            [],
                            new TypeRefNode(default, "void"),
                            new BlockStatementNode(default)
                        )
                    ]
                ),
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ExpressionStatementNode(
                            default,
                            new CallExpressionNode(
                                default,
                                new MemberAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(default, "Test"),
                                    "test"
                                ),
                                []
                            )
                        )
                    ])
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
                        new TypeRefNode(default, "i32"),
                        new FunctionTypeNode(
                            default,
                            [],
                            new DiscriminatedUnionNode([
                                new TypeRefNode(default, "i32"),
                                new TypeRefNode(default, "null"),
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
                        new TypeRefNode(default, "i32"),
                        new FunctionTypeNode(
                            default,
                            [],
                            new TypeRefNode(default, "i32")
                        ),
                        new TypeRefNode(default, "null"),
                    ])
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}