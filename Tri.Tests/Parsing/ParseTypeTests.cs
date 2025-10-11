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
        var (tree, diagnostics) = Parse("public type Point { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(21, 1, 22)),
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
        var (tree, diagnostics) = Parse("public type { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(15, 1, 16)),
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
            new SourceLocation(file, new SourcePosition(12, 1, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeMissingOpenBraceTest()
    {
        var (tree, diagnostics) = Parse("public type Point }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(19, 1, 20)),
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
            new SourceLocation(file, new SourcePosition(18, 1, 19).ToSpan()),
            "Expected '{'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse("public type Point {");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(19, 1, 20)),
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
            new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertiesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32;
                y: i32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 4, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(31, 2, 12)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11)),
                            "i32"
                        )
                    ),
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(36, 3, 5), new SourcePosition(43, 3, 12)),
                        "y",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(39, 3, 8), new SourcePosition(42, 3, 11)),
                            "i32"
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
    public void ParsePropertiesWithBlocksTest()
    {
        var (tree, diagnostics) = Parse(
            """
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

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(292, 18, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(155, 9, 6)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11)),
                            "i32"
                        ),
                        new PropertyGetterNode(
                            new SourceSpan(new SourcePosition(41, 3, 9), new SourcePosition(90, 5, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(53, 3, 21), new SourcePosition(90, 5, 10)),
                                [
                                    new ReturnStatementNode(
                                        new SourceSpan(new SourcePosition(67, 4, 13), new SourcePosition(80, 4, 26)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(74, 4, 20), new SourcePosition(79, 4, 25)),
                                            "field"
                                        )
                                    )
                                ])
                        ),
                        new PropertySetterNode(
                            new SourceSpan(new SourcePosition(99, 6, 9), new SourcePosition(149, 8, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(111, 6, 21), new SourcePosition(149, 8, 10)),
                                [
                                    new ExpressionStatementNode(
                                        new SourceSpan(new SourcePosition(125, 7, 13), new SourcePosition(139, 7, 27)),
                                        new BinaryExpressionNode(
                                            new SourceSpan(new SourcePosition(125, 7, 13), new SourcePosition(138, 7, 26)),
                                            BinaryExpressionKind.Assignment,
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(125, 7, 13), new SourcePosition(130, 7, 18)),
                                                "field"
                                            ),
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(133, 7, 21), new SourcePosition(138, 7, 26)),
                                                "value"
                                            )
                                        )
                                    )
                                ]
                            )
                        )
                    ),
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(160, 10, 5), new SourcePosition(290, 17, 6)),
                        "y",
                        new TypeNode(new SourceSpan(new SourcePosition(163, 10, 8), new SourcePosition(166, 10, 11)), "i32"),
                        new PropertyGetterNode(
                            new SourceSpan(new SourcePosition(177, 11, 9), new SourcePosition(225, 13, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(188, 11, 20), new SourcePosition(225, 13, 10)),
                                [
                                    new ReturnStatementNode(
                                        new SourceSpan(new SourcePosition(202, 12, 13), new SourcePosition(215, 12, 26)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(209, 12, 20), new SourcePosition(214, 12, 25)),
                                            "field"
                                        )
                                    )
                                ]
                            )
                        ),
                        new PropertySetterNode(
                            new SourceSpan(new SourcePosition(234, 14, 9), new SourcePosition(284, 16, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(246, 14, 21), new SourcePosition(284, 16, 10)),
                                [
                                    new ExpressionStatementNode(
                                        new SourceSpan(new SourcePosition(260, 15, 13), new SourcePosition(274, 15, 27)),
                                        new BinaryExpressionNode(
                                            new SourceSpan(new SourcePosition(260, 15, 13), new SourcePosition(273, 15, 26)),
                                            BinaryExpressionKind.Assignment,
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(260, 15, 13), new SourcePosition(265, 15, 18)),
                                                "field"
                                            ),
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(268, 15, 21), new SourcePosition(273, 15, 26)),
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
            public type Point {
                x: i32 {
                    private get;
                    private set {
                        field = value;
                    }
                }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(120, 8, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(118, 7, 6)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11)),
                            "i32"
                        ),
                        new PropertyGetterNode(
                            new SourceSpan(new SourcePosition(41, 3, 9), new SourcePosition(53, 3, 21)),
                            AccessModifier.Private,
                            null
                        ),
                        new PropertySetterNode(
                            new SourceSpan(new SourcePosition(62, 4, 9), new SourcePosition(112, 6, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(74, 4, 21), new SourcePosition(112, 6, 10)),
                                [
                                    new ExpressionStatementNode(
                                        new SourceSpan(new SourcePosition(88, 5, 13), new SourcePosition(102, 5, 27)),
                                        new BinaryExpressionNode(
                                            new SourceSpan(new SourcePosition(88, 5, 13), new SourcePosition(101, 5, 26)),
                                            BinaryExpressionKind.Assignment,
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(88, 5, 13), new SourcePosition(93, 5, 18)),
                                                "field"
                                            ),
                                            new MemberAccessExpressionNode(
                                                new SourceSpan(new SourcePosition(96, 5, 21), new SourcePosition(101, 5, 26)),
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
            public type Point {
                x: i32 {
                    private get {
                        return field;
                    }
                    private set;
                }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(119, 8, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(117, 7, 6)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11)), "i32"),
                        new PropertyGetterNode(
                            new SourceSpan(new SourcePosition(41, 3, 9), new SourcePosition(90, 5, 10)),
                            AccessModifier.Private,
                            new BlockStatementNode(new SourceSpan(new SourcePosition(53, 3, 21), new SourcePosition(90, 5, 10)), [
                                new ReturnStatementNode(
                                    new SourceSpan(new SourcePosition(67, 4, 13), new SourcePosition(80, 4, 26)),
                                    new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(74, 4, 20), new SourcePosition(79, 4, 25)), "field")
                                )
                            ])
                        ),
                        new PropertySetterNode(new SourceSpan(new SourcePosition(99, 6, 9), new SourcePosition(111, 6, 21)), AccessModifier.Private, null)
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
            public type Point {
                : i32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 3, 2)),
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
            new SourceLocation(file, new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(31, 3, 1))),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x i32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(30, 2, 11)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(26, 2, 7), new SourcePosition(29, 2, 10)),
                            "i32"
                        )
                    ),
                ],
                [],
                []
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(26, 2, 7).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: ;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(30, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(28, 2, 9)),
                        "x",
                        new FakeTypeNode(
                            new SourcePosition(27, 2, 8).ToSpan(),
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
            new SourceLocation(file, new SourcePosition(27, 2, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(31, 3, 1)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11)),
                            "i32"
                        )
                    ),
                ],
                [],
                []
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(31, 3, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): string { }

                public distance(other: Point): f32 { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(99, 5, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(53, 2, 34)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(49, 2, 30)), "string"),
                        new BlockStatementNode(new SourceSpan(new SourcePosition(50, 2, 31), new SourcePosition(53, 2, 34)), [])
                    ),
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(59, 4, 5), new SourcePosition(97, 4, 43)),
                        AccessModifier.Public,
                        false,
                        "distance",
                        [new ParameterNode(new SourceSpan(new SourcePosition(75, 4, 21), new SourcePosition(87, 4, 33)), "other", new TypeNode(new SourceSpan(new SourcePosition(82, 4, 28), new SourcePosition(87, 4, 33)), "Point"))],
                        new TypeNode(new SourceSpan(new SourcePosition(90, 4, 36), new SourcePosition(93, 4, 39)), "f32"),
                        new BlockStatementNode(new SourceSpan(new SourcePosition(94, 4, 40), new SourcePosition(97, 4, 43)), [])
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
            public type Point {
                public (): string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(45, 2, 26)),
                        AccessModifier.Public,
                        false,
                        "<>_0",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(35, 2, 16), new SourcePosition(41, 2, 22)),
                            "string"
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(42, 2, 23), new SourcePosition(45, 2, 26))
                        )
                    )
                ]
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0007ExpectedMethodName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(31, 2, 12).ToSpan()),
            "Expected a method name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString): string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(52, 2, 33)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(42, 2, 23), new SourcePosition(48, 2, 29)),
                            "string"
                        ),
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
            new SourceLocation(file, new SourcePosition(39, 2, 20).ToSpan()),
            "Expected '('.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(: string { return; }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(62, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(60, 2, 41)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(42, 2, 23), new SourcePosition(48, 2, 29)),
                            "string"
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(49, 2, 30), new SourcePosition(60, 2, 41)),
                            [
                                new ReturnStatementNode(
                                    new SourceSpan(new SourcePosition(51, 2, 32), new SourcePosition(58, 2, 39))
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
                new SourceLocation(file, new SourcePosition(40, 2, 21).ToSpan()),
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
            public type Point {
                public toString() string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(52, 2, 33)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(42, 2, 23), new SourcePosition(48, 2, 29)),
                            "string"
                        ),
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
            new SourceLocation(file, new SourcePosition(42, 2, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): { var x: i32 = 1; }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(64, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(62, 2, 43)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new InterfaceNode(
                            new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(45, 2, 26)),
                            [],
                            []
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(45, 2, 26), new SourcePosition(62, 2, 43)),
                            [
                                new VariableDeclarationNode(
                                    new SourceSpan(new SourcePosition(45, 2, 26), new SourcePosition(60, 2, 41)),
                                    "x",
                                    new TypeNode(
                                        new SourceSpan(new SourcePosition(52, 2, 33), new SourcePosition(55, 2, 36)),
                                        "i32"
                                    ),
                                    new LiteralExpressionNode(
                                        new SourceSpan(new SourcePosition(58, 2, 39), new SourcePosition(59, 2, 40)),
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
                new SourceLocation(file, new SourcePosition(45, 2, 26).ToSpan()),
                "Expected '}'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(45, 2, 26).ToSpan()),
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
            public type Point {
                public toString(): string }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(51, 2, 32)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(49, 2, 30)),
                            "string"
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(50, 2, 31), new SourcePosition(51, 2, 32))
                        )
                    )
                ]
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(50, 2, 31).ToSpan()),
            "Expected '{'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): string {
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(53, 3, 2)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(49, 2, 30)),
                            "string"
                        ),
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
            new SourceLocation(file, new SourcePosition(53, 3, 2).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCommaTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(a: i32 b: i32): string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(68, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(66, 2, 47)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [
                            new ParameterNode(
                                new SourceSpan(new SourcePosition(40, 2, 21), new SourcePosition(46, 2, 27)),
                                "a",
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(46, 2, 27)),
                                    "i32"
                                )
                            ),
                            new ParameterNode(
                                new SourceSpan(new SourcePosition(47, 2, 28), new SourcePosition(53, 2, 34)),
                                "b",
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(50, 2, 31), new SourcePosition(53, 2, 34)),
                                    "i32"
                                )
                            ),
                        ],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(56, 2, 37), new SourcePosition(62, 2, 43)),
                            "string"
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(63, 2, 44), new SourcePosition(66, 2, 47))
                        )
                    )
                ]
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(47, 2, 28), new SourcePosition(47, 2, 28))),
            "Expected ','.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(a i32): string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(60, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(58, 2, 39)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [
                            new ParameterNode(
                                new SourceSpan(new SourcePosition(40, 2, 21), new SourcePosition(45, 2, 26)),
                                "a",
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(42, 2, 23), new SourcePosition(45, 2, 26)),
                                    "i32"
                                )
                            ),
                        ],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(48, 2, 29), new SourcePosition(54, 2, 35)),
                            "string"
                        ),
                        new BlockStatementNode(
                            new SourceSpan(new SourcePosition(55, 2, 36), new SourcePosition(58, 2, 39))
                        )
                    )
                ]
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(42, 2, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(a: ): string { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(58, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(56, 2, 37)),
                        AccessModifier.Public,
                        false,
                        "toString",
                        [
                            new ParameterNode(
                                new SourceSpan(new SourcePosition(40, 2, 21), new SourcePosition(43, 2, 24)),
                                "a",
                                new FakeTypeNode(
                                    new SourcePosition(43, 2, 24).ToSpan(),
                                    "<>_0"
                                )
                            ),
                        ],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(46, 2, 27), new SourcePosition(52, 2, 33)),
                            "string"
                        ),
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
            new SourceLocation(file, new SourcePosition(43, 2, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) { }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(64, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [
                    new ConstructorDeclarationNode(
                        new SourceSpan(new SourcePosition(24, 2, 5), new SourcePosition(62, 2, 43)),
                        AccessModifier.Public,
                        [
                            new ParameterNode(new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(49, 2, 30)), "x", new TypeNode(new SourceSpan(new SourcePosition(46, 2, 27), new SourcePosition(49, 2, 30)), "i32")),
                            new ParameterNode(new SourceSpan(new SourcePosition(51, 2, 32), new SourcePosition(57, 2, 38)), "y", new TypeNode(new SourceSpan(new SourcePosition(54, 2, 35), new SourcePosition(57, 2, 38)), "i32")),
                        ],
                        new BlockStatementNode(new SourceSpan(new SourcePosition(59, 2, 40), new SourcePosition(62, 2, 43)), [])
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
        var (tree, diagnostics) = Parse("public type Point : Interface1, Interface2 { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(46, 1, 47)),
                AccessModifier.Public,
                "Point",
                [],
                [new TypeNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(30, 1, 31)), "Interface1"), new TypeNode(new SourceSpan(new SourcePosition(32, 1, 33), new SourcePosition(42, 1, 43)), "Interface2")],
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
        var (tree, diagnostics) = Parse("public type Point : { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
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
            new SourceLocation(file, new SourcePosition(20, 1, 21).ToSpan()),
            "Expected an interface.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingCommaInInterfacesTest()
    {
        var (tree, diagnostics) = Parse("public type Point : Interface1 Interface2 { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 1, 46)),
                AccessModifier.Public,
                "Point",
                [],
                [
                    new TypeNode(
                        new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(30, 1, 31)),
                        "Interface1"
                    ),
                    new TypeNode(
                        new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(41, 1, 42)),
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
            new SourceLocation(file, new SourcePosition(31, 1, 32).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingSecondInterfaceTest()
    {
        var (tree, diagnostics) = Parse("public type Point : Interface1, { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(35, 1, 36)),
                AccessModifier.Public,
                "Point",
                [],
                [
                    new TypeNode(
                        new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(30, 1, 31)),
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
            new SourceLocation(file, new SourcePosition(32, 1, 33).ToSpan()),
            "Expected an interface.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type MyType = i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(25, 1, 26)),
                AccessModifier.Public,
                "MyType",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32")
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseTypeAliasMissingNameTest()
    {
        var (tree, diagnostics) = Parse("public type = i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(18, 1, 19)),
                AccessModifier.Public,
                "<>_0",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(17, 1, 18)),
                    "i32"
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0005ExpectedTypeName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(12, 1, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("public type MyType = ;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23)),
                AccessModifier.Public,
                "MyType",
                [],
                new FakeTypeNode(
                    new SourcePosition(21, 1, 22).ToSpan(),
                    "<>_0"
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(21, 1, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse("public type MyType = i32");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(24, 1, 25)),
                AccessModifier.Public,
                "MyType",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)),
                    "i32"
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(24, 1, 25).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse("public type F = () => void;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(26, 1, 27)),
                    [],
                    new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(26, 1, 27)), "void")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeWithParametersTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32) => i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 1, 35)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(33, 1, 34)),
                    [new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32")],
                    new TypeNode(new SourceSpan(new SourcePosition(30, 1, 31), new SourcePosition(33, 1, 34)), "i32")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeMissingNameTest()
    {
        var (tree, diagnostics) = Parse("public type = (i32, i32) => i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
                AccessModifier.Public,
                "<>_0",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(31, 1, 32)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(23, 1, 24)),
                            "i32"
                        )
                    ],
                    new TypeNode(
                        new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)),
                        "i32"
                    )
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0005ExpectedTypeName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(12, 1, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeMissingEqualTest()
    {
        var (tree, diagnostics) = Parse("public type F (i32, i32) => i32;");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
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
                new SourceLocation(file, new SourcePosition(14, 1, 15).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0014ExpectedTypeMember,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(32, 1, 33))),
                "Expected a type member (a property, a method or a constructor).")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse("public type F = i32, i32) => i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(19, 1, 20)),
                AccessModifier.Public,
                "F",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)),
                    "i32"
                )
            ),
            new FakeDeclarationNode(
                new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(33, 1, 34))
            )
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(33, 1, 34))),
                "Expected a type or a function.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32 => i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(33, 1, 34)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(32, 1, 33)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)),
                            "i32"
                        )
                    ],
                    new TypeNode(
                        new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)),
                        "i32"
                    )
                )
            )
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(26, 1, 27).ToSpan()),
                "Expected ')'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeCommaTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32 i32) => i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(33, 1, 34)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(32, 1, 33)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)),
                            "i32"
                        )
                    ],
                    new TypeNode(
                        new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)),
                        "i32"
                    )
                )
            )
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(21, 1, 22).ToSpan()),
                "Expected ','.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeArrowTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32) i32;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                AccessModifier.Public,
                "F",
                [],
                new TupleTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(26, 1, 27)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)),
                            "i32"
                        ),
                    ]
                )
            ),
            new FakeDeclarationNode(
                new SourceSpan(new SourcePosition(27, 1, 28), new SourcePosition(31, 1, 32))
            ),
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(27, 1, 28).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(27, 1, 28), new SourcePosition(31, 1, 32))),
                "Expected a type or a function.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32) => ;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(30, 1, 31)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)),
                            "i32"
                        )
                    ],
                    new FakeTypeNode(
                        new SourcePosition(30, 1, 31).ToSpan(),
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
                new SourceLocation(file, new SourcePosition(30, 1, 31).ToSpan()),
                "Expected a type.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeSemiColonTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32) => i32");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(33, 1, 34)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(33, 1, 34)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)),
                            "i32"
                        )
                    ],
                    new TypeNode(
                        new SourceSpan(new SourcePosition(30, 1, 31), new SourcePosition(33, 1, 34)),
                        "i32"
                    )
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(33, 1, 34).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeInParameterTest()
    {
        var (tree, diagnostics) = Parse("public test(callback: (i32, i32) => void): void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 1, 52)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(40, 1, 41)),
                        "callback",
                        new FunctionTypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(40, 1, 41)),
                            [
                                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32"),
                                new TypeNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)), "i32")
                            ],
                            new TypeNode(new SourceSpan(new SourcePosition(36, 1, 37), new SourcePosition(40, 1, 41)), "void")
                        )
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(43, 1, 44), new SourcePosition(47, 1, 48)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(48, 1, 49), new SourcePosition(51, 1, 52)), [])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeInReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("public test(): (i32, i32) => void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(37, 1, 38)),
                AccessModifier.Public,
                "test",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(33, 1, 34)),
                    [
                        new TypeNode(
                            new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)),
                            "i32"
                        ),
                        new TypeNode(
                            new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)),
                            "i32"
                        )
                    ],
                    new TypeNode(
                        new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(33, 1, 34)),
                        "void"
                    )
                ),
                new BlockStatementNode(new SourceSpan(new SourcePosition(34, 1, 35), new SourcePosition(37, 1, 38)), [])
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
            public main(): void {
                var x: (i32, i32) => void = 0;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(58, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(58, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(56, 2, 35)),
                        "x",
                        new FunctionTypeNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(51, 2, 30)),
                            [new TypeNode(new SourceSpan(new SourcePosition(34, 2, 13), new SourcePosition(37, 2, 16)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(42, 2, 21)), "i32")],
                            new TypeNode(new SourceSpan(new SourcePosition(47, 2, 26), new SourcePosition(51, 2, 30)), "void")
                        ),
                        LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(54, 2, 33), new SourcePosition(55, 2, 34)), 0)
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
            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(74, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(74, 6, 2)),
                    [
                        new InterfacePropertyNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)), "x", new TypeNode(new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)), "i32"), null, null),
                        new InterfacePropertyNode(new SourceSpan(new SourcePosition(38, 3, 5), new SourcePosition(45, 3, 12)), "y", new TypeNode(new SourceSpan(new SourcePosition(41, 3, 8), new SourcePosition(44, 3, 11)), "i32"), null, null)
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(51, 5, 5), new SourcePosition(72, 5, 26)),
                            "distance",
                            [new TypeNode(new SourceSpan(new SourcePosition(60, 5, 14), new SourcePosition(65, 5, 19)), "Point")],
                            new TypeNode(new SourceSpan(new SourcePosition(68, 5, 22), new SourcePosition(71, 5, 25)), "f32")
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
            public type Point = {
                x: i32 { public get; public set; }
                y: i32 { private get; private set; }

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(130, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(130, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(60, 2, 39)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)), "i32"),
                            AccessModifier.Public,
                            AccessModifier.Public
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(65, 3, 5), new SourcePosition(101, 3, 41)),
                            "y",
                            new TypeNode(new SourceSpan(new SourcePosition(68, 3, 8), new SourcePosition(71, 3, 11)), "i32"),
                            AccessModifier.Private,
                            AccessModifier.Private
                        )
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(107, 5, 5), new SourcePosition(128, 5, 26)),
                            "distance",
                            [new TypeNode(new SourceSpan(new SourcePosition(116, 5, 14), new SourcePosition(121, 5, 19)), "Point")],
                            new TypeNode(new SourceSpan(new SourcePosition(124, 5, 22), new SourcePosition(127, 5, 25)), "f32")
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
            public type Point = {
                x: i32 { public get; }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(50, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(50, 3, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(48, 2, 27)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)), "i32"),
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
            public type Point = {
                x: i32 { public set; }
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(50, 3, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(50, 3, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(48, 2, 27)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)), "i32"),
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
            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(72, 5, 26)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(72, 5, 26)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(38, 3, 5), new SourcePosition(45, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(41, 3, 8), new SourcePosition(44, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(51, 5, 5), new SourcePosition(72, 5, 26)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(60, 5, 14), new SourcePosition(65, 5, 19)),
                                    "Point"
                                )
                            ],
                            new TypeNode(
                                new SourceSpan(new SourcePosition(68, 5, 22), new SourcePosition(71, 5, 25)),
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
            new SourceLocation(file, new SourcePosition(72, 5, 26).ToSpan()),
            "Expected '}'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertyTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: ;
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(71, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(71, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(30, 2, 9)),
                            "x",
                            new FakeTypeNode(
                                new SourcePosition(29, 2, 8).ToSpan(),
                                "<>_0"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(35, 3, 5), new SourcePosition(42, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(38, 3, 8), new SourcePosition(41, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(48, 5, 5), new SourcePosition(69, 5, 26)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(57, 5, 14), new SourcePosition(62, 5, 19)),
                                    "Point"
                                )
                            ],
                            new TypeNode(
                                new SourceSpan(new SourcePosition(65, 5, 22), new SourcePosition(68, 5, 25)),
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
            new SourceLocation(file, new SourcePosition(29, 2, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertySemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(73, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(73, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(37, 3, 5)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(37, 3, 5), new SourcePosition(44, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(40, 3, 8), new SourcePosition(43, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(50, 5, 5), new SourcePosition(71, 5, 26)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(59, 5, 14), new SourcePosition(64, 5, 19)),
                                    "Point"
                                )
                            ],
                            new TypeNode(
                                new SourceSpan(new SourcePosition(67, 5, 22), new SourcePosition(70, 5, 25)),
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
                new SourceLocation(file, new SourcePosition(37, 3, 5).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(37, 3, 5).ToSpan()),
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
            public type Point = {
                x: i32;
                y: i32;

                distance(Point): ;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(71, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(71, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(38, 3, 5), new SourcePosition(45, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(41, 3, 8), new SourcePosition(44, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(51, 5, 5), new SourcePosition(69, 5, 23)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(60, 5, 14), new SourcePosition(65, 5, 19)),
                                    "Point"
                                )
                            ],
                            new FakeTypeNode(
                                new SourcePosition(68, 5, 22).ToSpan(),
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
            new SourceLocation(file, new SourcePosition(68, 5, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(Point) f64;
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(73, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(73, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(38, 3, 5), new SourcePosition(45, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(41, 3, 8), new SourcePosition(44, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(51, 5, 5), new SourcePosition(71, 5, 25)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(60, 5, 14), new SourcePosition(65, 5, 19)),
                                    "Point"
                                )
                            ],
                            new TypeNode(
                                new SourceSpan(new SourcePosition(67, 5, 21), new SourcePosition(70, 5, 24)),
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
            new SourceLocation(file, new SourcePosition(67, 5, 21).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f64
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(73, 6, 2)),
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(73, 6, 2)),
                    [
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 2, 8), new SourcePosition(32, 2, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                        new InterfacePropertyNode(
                            new SourceSpan(new SourcePosition(38, 3, 5), new SourcePosition(45, 3, 12)),
                            "y",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(41, 3, 8), new SourcePosition(44, 3, 11)),
                                "i32"
                            ),
                            null,
                            null
                        ),
                    ],
                    [
                        new InterfaceMethodNode(
                            new SourceSpan(new SourcePosition(51, 5, 5), new SourcePosition(72, 6, 1)),
                            "distance",
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(60, 5, 14), new SourcePosition(65, 5, 19)),
                                    "Point"
                                )
                            ],
                            new TypeNode(
                                new SourceSpan(new SourcePosition(68, 5, 22), new SourcePosition(71, 5, 25)),
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
            new SourceLocation(file, new SourcePosition(72, 6, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var p: Point = new Point();
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(53, 2, 32)),
                        "p",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)), "Point"),
                        new NewObjectExpressionNode(new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(52, 2, 31)), new TypeNode(new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(50, 2, 29)), "Point"), [])
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
            public main(): void {
                var p: Point = new Point(1, 2);
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(59, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(59, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(57, 2, 36)),
                        "p",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)), "Point"),
                        new NewObjectExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(56, 2, 35)),
                            new TypeNode(new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(50, 2, 29)), "Point"),
                            [
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(51, 2, 30), new SourcePosition(52, 2, 31)), 1),
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(54, 2, 33), new SourcePosition(55, 2, 34)), 2)
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
            public main(): void {
                var p: Point = new ();
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(50, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(50, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(48, 2, 27)),
                            "p",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                                "Point"
                            ),
                            new NewObjectExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(47, 2, 26)),
                                new FakeTypeNode(
                                    new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(45, 2, 24)),
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
            new SourceLocation(file, new SourcePosition(45, 2, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingArgumentTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var p: Point = new Point(1, );
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(58, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(58, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(56, 2, 35)),
                            "p",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                                "Point"
                            ),
                            new NewObjectExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(55, 2, 34)),
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(50, 2, 29)),
                                    "Point"
                                ),
                                [
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(51, 2, 30), new SourcePosition(52, 2, 31)),
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
            new SourceLocation(file, new SourcePosition(54, 2, 33).ToSpan()),
            "Expected an expression.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var p: Point = new Point(;
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(54, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(52, 2, 31)),
                            "p",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                                "Point"
                            ),
                            new NewObjectExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(51, 2, 30)),
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(50, 2, 29)),
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
            new SourceLocation(file, new SourcePosition(51, 2, 30).ToSpan()),
            "Expected ')'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse("public type T = { } | i32 | () => void;");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(39, 1, 40)),
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new InterfaceNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), [], []),
                    new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32"),
                    new FunctionTypeNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(38, 1, 39)), [], new TypeNode(new SourceSpan(new SourcePosition(34, 1, 35), new SourcePosition(38, 1, 39)), "void")),
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
            public main(): void {
                var x: i32 | null = null;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(53, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(51, 2, 30)),
                        "x",
                        new DiscriminatedUnionNode([
                            new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                            new TypeNode(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(43, 2, 22)), "null")
                        ]),
                        new NullExpressionNode(new SourceSpan(new SourcePosition(46, 2, 25), new SourcePosition(50, 2, 29)))
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
        var (tree, diagnostics) = Parse("public type T = (i32, i32);");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(26, 1, 27)), [new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32")])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void NestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = ((i32, i32), i32);");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 1, 35)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(33, 1, 34)), [
                    new TupleTypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(27, 1, 28)), [new TypeNode(new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(21, 1, 22)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32")]),
                    new TypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)), "i32")
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeWithDuTest()
    {
        var (tree, diagnostics) = Parse("public type T = (bool | i32, () => void);");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 1, 42)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(40, 1, 41)),
                    [
                        new DiscriminatedUnionNode([
                            new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "bool"),
                            new TypeNode(new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(27, 1, 28)), "i32")
                        ]),
                        new FunctionTypeNode(
                            new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(39, 1, 40)),
                            [],
                            new TypeNode(new SourceSpan(new SourcePosition(35, 1, 36), new SourcePosition(39, 1, 40)), "void")
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
        var (tree, diagnostics) = Parse("public type T = (i32);");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23)),
                AccessModifier.Public,
                "T",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                    "i32"
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeMissingTest()
    {
        const string code = "public type T = (i32";

        var (tree, diagnostics) = Parse(code);

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(20, 1, 21)),
                AccessModifier.Public,
                "T",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                    "i32"
                )
            )
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(20, 1, 21).ToSpan()),
                "Expected ')'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(20, 1, 21).ToSpan()),
                "Expected ';'.")
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void FunctionWithTupleTest()
    {
        var (tree, diagnostics) = Parse("public main(): (i32, i32) { }");
        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(29, 1, 30)),
                AccessModifier.Public,
                "main",
                [],
                new TupleTypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(25, 1, 26)),
                    [
                        new TypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), "i32"),
                        new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32")
                    ]
                ),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(29, 1, 30)), [])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleInDuTest()
    {
        var (tree, diagnostics) = Parse("public type T = i32 | (bool, f64);");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 1, 35)),
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new TypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), "i32"),
                    new TupleTypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(33, 1, 34)), [new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "bool"), new TypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)), "f64")])
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var (tree, diagnostics) = Parse("public type T = (bool, i32 | f64);");
        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 1, 35)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(33, 1, 34)), [
                    new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "bool"),
                    new DiscriminatedUnionNode([new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)), "f64")])
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
            public type Test {
                public static test(): void { }
            }
            """);
        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "Test",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(23, 2, 5), new SourcePosition(53, 2, 35)),
                        AccessModifier.Public,
                        true,
                        "test",
                        [],
                        new TypeNode(new SourceSpan(new SourcePosition(45, 2, 27), new SourcePosition(49, 2, 31)), "void"),
                        new BlockStatementNode(new SourceSpan(new SourcePosition(50, 2, 32), new SourcePosition(53, 2, 35)))
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
            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """);

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "Test",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        new SourceSpan(new SourcePosition(23, 2, 5), new SourcePosition(53, 2, 35)),
                        AccessModifier.Public,
                        true,
                        "test",
                        [],
                        new TypeNode(new SourceSpan(new SourcePosition(45, 2, 27), new SourcePosition(49, 2, 31)), "void"),
                        new BlockStatementNode(new SourceSpan(new SourcePosition(50, 2, 32), new SourcePosition(53, 2, 35)))
                    )
                ]
            ),
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(57, 5, 1), new SourcePosition(97, 7, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(72, 5, 16), new SourcePosition(76, 5, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(77, 5, 21), new SourcePosition(97, 7, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(83, 6, 5), new SourcePosition(95, 6, 17)),
                        new CallExpressionNode(
                            new SourceSpan(new SourcePosition(83, 6, 5), new SourcePosition(94, 6, 16)),
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(83, 6, 5), new SourcePosition(92, 6, 14)),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(83, 6, 5), new SourcePosition(87, 6, 9)), "Test"),
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
        var (tree, diagnostics) = Parse("public type T = i32 | (() => i32 | null);");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 1, 42)),
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new TypeNode(
                        new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)),
                        "i32"
                    ),
                    new FunctionTypeNode(
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(39, 1, 40)),
                        [],
                        new DiscriminatedUnionNode([
                            new TypeNode(
                                new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)),
                                "i32"
                            ),
                            new TypeNode(
                                new SourceSpan(new SourcePosition(35, 1, 36), new SourcePosition(39, 1, 40)),
                                "null"
                            ),
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
        var (tree, diagnostics) = Parse("public type T = i32 | (() => i32) | null;");

        var expected = new SyntaxTree(file, [
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 1, 42)),
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new TypeNode(
                        new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)),
                        "i32"
                    ),
                    new FunctionTypeNode(
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(32, 1, 33)),
                        [],
                        new TypeNode(
                            new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(32, 1, 33)),
                            "i32"
                        )
                    ),
                    new TypeNode(
                        new SourceSpan(new SourcePosition(36, 1, 37), new SourcePosition(40, 1, 41)),
                        "null"
                    ),
                ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}