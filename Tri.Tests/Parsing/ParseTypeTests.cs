using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeTests
{
    [Test]
    public void ParseTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type Point { }");

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseTypeMissingTypeKeywordTest()
    {
        var parser = new Parser();
        const string code = "public Point { }";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingNameTest()
    {
        var parser = new Parser();
        const string code = "public type { }";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingOpenBraceTest()
    {
        var parser = new Parser();
        const string code = "public type Point }";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code = "public type Point {";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParsePropertiesTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point {
                x: i32;
                y: i32;
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParsePropertiesWithBlocksTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
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

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseEmptyGetterTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
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

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseEmptySetterTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
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

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParsePropertyMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private : i32;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParsePropertyMissingColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x i32;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParsePropertyMissingTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x: ;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParsePropertyMissingSemiColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x: i32
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodsTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point {
                public toString(): string { }

                public distance(other: Point): f32 { }
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseMethodMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public (): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingOpenParenTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCloseParenTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(: string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString() string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingReturnTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingOpenBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): string }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): string {
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCommaTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a: i32 b: i32): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingParameterColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a i32): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingParameterTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a: ): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseCtorTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) { }
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseTypeWithInterfaceTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point : Interface1, Interface2 { }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseTypeWithMissingInterfaceTest()
    {
        var parser = new Parser();
        const string code = "public type Point : { }";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeWithMissingSecondInterfaceTest()
    {
        var parser = new Parser();
        const string code = "public type Point : Interface1, { }";

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeAliasTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type MyType = i32;");

        var expected = new SyntaxTree([
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
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type = i32;"));
    }

    [Test]
    public void ParseTypeAliasMissingTypeTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type MyType = ;"));
    }

    [Test]
    public void ParseTypeAliasMissingSemiColonTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type MyType = i32"));
    }

    [Test]
    public void ParseFunctionTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type F = () => void;");

        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(18, 1, 19)),
                    [],
                    new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(26, 1, 27)), "void")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseFunctionTypeWithParametersTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type F = (i32, i32) => i32;");

        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 1, 35)),
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(26, 1, 27)),
                    [new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32")],
                    new TypeNode(new SourceSpan(new SourcePosition(30, 1, 31), new SourcePosition(33, 1, 34)), "i32")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseFunctionTypeMissingNameTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type = (i32, i32) => i32;"));
    }

    [Test]
    public void ParseFunctionTypeMissingEqualTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F (i32, i32) => i32;"));
    }

    [Test]
    public void ParseFunctionTypeMissingOpenParenTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = i32, i32) => i32;"));
    }

    [Test]
    public void ParseFunctionTypeMissingCloseParenTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = (i32, i32 => i32;"));
    }

    [Test]
    public void ParseFunctionTypeCommaTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = (i32 i32) => i32;"));
    }

    [Test]
    public void ParseFunctionTypeArrowTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = (i32, i32) i32;"));
    }

    [Test]
    public void ParseFunctionTypeReturnTypeTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = (i32, i32) => ;"));
    }

    [Test]
    public void ParseFunctionTypeSemiColonTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public type F = (i32, i32) => i32"));
    }

    [Test]
    public void ParseFunctionTypeInParameterTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(callback: (i32, i32) => void): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 1, 52)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(32, 1, 33)),
                        "callback",
                        new FunctionTypeNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(32, 1, 33)),
                            [new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)), "i32")],
                            new TypeNode(new SourceSpan(new SourcePosition(36, 1, 37), new SourcePosition(40, 1, 41)), "void")
                        )
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(43, 1, 44), new SourcePosition(47, 1, 48)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(48, 1, 49), new SourcePosition(51, 1, 52)), [])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseFunctionTypeInReturnTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(): (i32, i32) => void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(37, 1, 38)),
                AccessModifier.Public,
                "test",
                [],
                new FunctionTypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(25, 1, 26)),
                    [new TypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32")],
                    new TypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(33, 1, 34)), "void")
                ),
                new BlockStatementNode(new SourceSpan(new SourcePosition(34, 1, 35), new SourcePosition(37, 1, 38)), [])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseFunctionTypeInVariableTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public main(): void {
                var x: (i32, i32) => void = 0;
            }
            """);

        var expected = new SyntaxTree([
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
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(43, 2, 22)),
                            [new TypeNode(new SourceSpan(new SourcePosition(34, 2, 13), new SourcePosition(37, 2, 16)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(42, 2, 21)), "i32")],
                            new TypeNode(new SourceSpan(new SourcePosition(47, 2, 26), new SourcePosition(51, 2, 30)), "void")
                        ),
                        LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(54, 2, 33), new SourcePosition(55, 2, 34)), 0)
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseAliasInterfaceTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseAliasInterfaceTypeWithGettersSettersTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point = {
                x: i32 { public get; public set; }
                y: i32 { private get; private set; }

                distance(Point): f32;
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(other: Point): f32;
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertyTypeTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: ;
                y: i32;

                distance(other: Point): f32;
            }
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertySemiColonTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: i32
                y: i32;

                distance(other: Point): f32;
            }
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodReturnTypeTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(other: Point): ;
            }
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodColonTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(other: Point) f64;
            }
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodSemiColonTest()
    {
        var parser = new Parser();
        const string code =
            """
            public type Point = {
                x: i32;
                y: i32;

                distance(other: Point): f64
            }
            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseNewOperatorTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public main(): void {
                var p: Point = new Point();
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseNewOperatorWithParametersTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public main(): void {
                var p: Point = new Point(1, 2);
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseNewOperatorMissingTypeTest()
    {
        var parser = new Parser();
        const string code =
            """
            public main(): void {
                var p: Point = new ();
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseNewOperatorMissingArgumentTest()
    {
        var parser = new Parser();
        const string code =
            """
            public main(): void {
                var p: Point = new Point(1, );
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an argument."));
    }

    [Test]
    public void ParseNewOperatorMissingCloseParenTest()
    {
        var parser = new Parser();
        const string code =
            """
            public main(): void {
                var p: Point = new Point(;
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close parenthesis."));
    }

    [Test]
    public void ParseDiscriminatedUnionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = { } | i32 | () => void;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(39, 1, 40)),
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new InterfaceNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), [], []),
                    new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32"),
                    new FunctionTypeNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(30, 1, 31)), [], new TypeNode(new SourceSpan(new SourcePosition(34, 1, 35), new SourcePosition(38, 1, 39)), "void")),
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseNullTest()
    {
        const string code =
            """
            public main(): void {
                var x: i32 | null = null;
            }
            """;

        var parser = new Parser();
        var tree = parser.Parse(code);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void TupleTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (i32, i32);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(26, 1, 27)), [new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)), "i32")])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void NestedTupleTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = ((i32, i32), i32);");
        var expected = new SyntaxTree([
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
    }

    [Test]
    public void TupleTypeWithDuTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (bool | i32, () => void);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 1, 42)),
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(40, 1, 41)), [
                    new DiscriminatedUnionNode([new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "bool"), new TypeNode(new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(27, 1, 28)), "i32")]),
                    new FunctionTypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(31, 1, 32)), [], new TypeNode(new SourceSpan(new SourcePosition(35, 1, 36), new SourcePosition(39, 1, 40)), "void"))
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void TupleTypeWithSingleTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (i32);");

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void TupleTypeMissingTest()
    {
        var parser = new Parser();
        const string code = "public type T = (i32";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close parenthesis."));
    }

    [Test]
    public void FunctionWithTupleTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public main(): (i32, i32) { }");
        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseTupleInDuTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = i32 | (bool, f64);");
        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (bool, i32 | f64);");
        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseStaticMethodTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Test {
                public static test(): void { }
            }
            """);
        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseCallStaticMethodTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """);

        var expected = new SyntaxTree([
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
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDuTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = i32 | (() => i32 | null);");

        var expected = new SyntaxTree([
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
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)),
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
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDu2Test()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = i32 | (() => i32) | null;");

        var expected = new SyntaxTree([
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
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)),
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
    }
}