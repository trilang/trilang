using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeTests
{
    [Test]
    public void ParseTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point { }
            """);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseTypeMissingTypeKeywordTest()
    {
        var parser = new Parser();
        const string code = """
                            public Point { }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type { }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingOpenBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                            """;

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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode("x", new TypeNode("i32")),
                    new PropertyDeclarationNode("y", new TypeNode("i32")),
                ],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32"),
                        new PropertyGetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode("field")
                                )
                            ])
                        ),
                        new PropertySetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("field"),
                                        new MemberAccessExpressionNode("value")
                                    )
                                )
                            ])
                        )
                    ),
                    new PropertyDeclarationNode(
                        "y",
                        new TypeNode("i32"),
                        new PropertyGetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode("field")
                                )
                            ])
                        ),
                        new PropertySetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("field"),
                                        new MemberAccessExpressionNode("value")
                                    )
                                )
                            ])
                        )
                    ),
                ],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32"),
                        new PropertyGetterNode(AccessModifier.Private, null),
                        new PropertySetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("field"),
                                        new MemberAccessExpressionNode("value")
                                    )
                                )
                            ])
                        )
                    ),
                ],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32"),
                        new PropertyGetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode("field")
                                )
                            ])
                        ),
                        new PropertySetterNode(AccessModifier.Private, null)
                    ),
                ],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "toString",
                        [],
                        new TypeNode("string"),
                        new BlockStatementNode([])
                    ),
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "distance",
                        [new ParameterNode("other", new TypeNode("Point"))],
                        new TypeNode("f32"),
                        new BlockStatementNode([])
                    )
                ]
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [],
                [],
                [
                    new ConstructorDeclarationNode(
                        AccessModifier.Public,
                        [
                            new ParameterNode("x", new TypeNode("i32")),
                            new ParameterNode("y", new TypeNode("i32")),
                        ],
                        new BlockStatementNode([])
                    )
                ],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                [new TypeNode("Interface1"), new TypeNode("Interface2")],
                [],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "MyType",
                [],
                new TypeNode("i32")
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    [],
                    new TypeNode("void")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseFunctionTypeWithParametersTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type F = (i32, i32) => i32;");

        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "F",
                [],
                new FunctionTypeNode(
                    [new TypeNode("i32"), new TypeNode("i32")],
                    new TypeNode("i32")
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
        var tree = parser.Parse(
            """
            function test(callback: (i32, i32) => void): void { }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [
                    new ParameterNode(
                        "callback",
                        new FunctionTypeNode(
                            [new TypeNode("i32"), new TypeNode("i32")],
                            new TypeNode("void")))
                ],
                new TypeNode("void"),
                new BlockStatementNode([])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseFunctionTypeInReturnTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(): (i32, i32) => void { }");
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [],
                new FunctionTypeNode(
                    [new TypeNode("i32"), new TypeNode("i32")],
                    new TypeNode("void")),
                new BlockStatementNode([])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseFunctionTypeInVariableTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                var x: (i32, i32) => void = 0;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new FunctionTypeNode(
                            [new TypeNode("i32"), new TypeNode("i32")],
                            new TypeNode("void")),
                        LiteralExpressionNode.Number(0)
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    [
                        new InterfacePropertyNode("x", new TypeNode("i32"), null, null),
                        new InterfacePropertyNode("y", new TypeNode("i32"), null, null)
                    ],
                    [
                        new InterfaceMethodNode(
                            "distance",
                            [new TypeNode("Point")],
                            new TypeNode("f32"))
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    [
                        new InterfacePropertyNode("x", new TypeNode("i32"), AccessModifier.Public, AccessModifier.Public),
                        new InterfacePropertyNode("y", new TypeNode("i32"), AccessModifier.Private, AccessModifier.Private)
                    ],
                    [
                        new InterfaceMethodNode(
                            "distance",
                            [new TypeNode("Point")],
                            new TypeNode("f32"))
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
            function main(): void {
                var p: Point = new Point();
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "p",
                        new TypeNode("Point"),
                        new NewObjectExpressionNode(new TypeNode("Point"), [])
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseNewOperatorWithParametersTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                var p: Point = new Point(1, 2);
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "p",
                        new TypeNode("Point"),
                        new NewObjectExpressionNode(
                            new TypeNode("Point"),
                            [LiteralExpressionNode.Number(1), LiteralExpressionNode.Number(2)]
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }


    [Test]
    public void ParseNewOperatorMissingTypeTest()
    {
        var parser = new Parser();
        const string code =
            """
            function main(): void {
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
            function main(): void {
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
            function main(): void {
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
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new InterfaceNode([], []),
                    new TypeNode("i32"),
                    new FunctionTypeNode([], new TypeNode("void")),
                ]))
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseNullTest()
    {
        const string code =
            """
            function main(): void {
                var x: i32 | null = null;
            }
            """;

        var parser = new Parser();
        var tree = parser.Parse(code);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new DiscriminatedUnionNode([new TypeNode("i32"), new TypeNode("null")]),
                        new NullExpressionNode()
                    )
                ]))
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void TupleTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (i32, i32);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode([new TypeNode("i32"), new TypeNode("i32")])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void NestedTupleTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = ((i32, i32), i32);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode([
                    new TupleTypeNode([new TypeNode("i32"), new TypeNode("i32")]),
                    new TypeNode("i32")
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void TupleTypeWithDuTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (bool | i32, () => void);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode([
                    new DiscriminatedUnionNode([new TypeNode("bool"), new TypeNode("i32")]),
                    new FunctionTypeNode([], new TypeNode("void"))
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void TupleTypeWithSingleTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T = (i32);";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
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
        var tree = parser.Parse("function main(): (i32, i32) { }");
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TupleTypeNode([new TypeNode("i32"), new TypeNode("i32")]),
                new BlockStatementNode([])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseTupleInDuTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = i32 | (bool, f64);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new TypeNode("i32"),
                    new TupleTypeNode([new TypeNode("bool"), new TypeNode("f64")])
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = (bool, i32 | f64);");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode([
                    new TypeNode("bool"),
                    new DiscriminatedUnionNode([new TypeNode("i32"), new TypeNode("f64")])
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}