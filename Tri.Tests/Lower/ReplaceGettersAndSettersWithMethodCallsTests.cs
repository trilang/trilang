using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class ReplaceGettersAndSettersWithMethodCallsTests
{
    private static SemanticTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return semanticTree;
    }

    [Test]
    public void ReplaceReadAccessWithGetterMethodCall()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32;
            }

            public test(p: Point): i32 {
                return p.x;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var point = new TypeMetadata("Point");
        point.AddConstructor(
            new ConstructorMetadata(
                point,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], point)));

        var count = new PropertyMetadata(point, "x", TypeMetadata.I32);
        point.AddProperty(count);
        point.AddMethod(count.Getter!);
        point.AddMethod(count.Setter!);

        var backingField = new FieldMetadata(point, $"<>_{count.Name}", count.Type);
        point.AddField(backingField);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var pointType = new TypeMetadata("Point");
        pointType.AddConstructor(
            new ConstructorMetadata(
                pointType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], pointType)));

        var xProperty = new PropertyMetadata(
            pointType,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        pointType.AddProperty(xProperty);
        pointType.AddMethod(xProperty.Getter!);
        pointType.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(pointType, $"<>_{xProperty.Name}", xProperty.Type);
        pointType.AddField(xBackingField);

        var testType = new TypeMetadata("Test");
        testType.AddConstructor(
            new ConstructorMetadata(
                testType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], testType)));

        var pointProperty = new PropertyMetadata(testType, "point", pointType);
        testType.AddProperty(pointProperty);
        testType.AddMethod(pointProperty.Getter!);
        testType.AddMethod(pointProperty.Setter!);

        var pointBackingField = new FieldMetadata(testType, $"<>_{pointProperty.Name}", pointProperty.Type);
        testType.AddField(pointBackingField);

        var tParameter = new ParameterMetadata("t", testType);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [tParameter],
            new FunctionTypeMetadata([testType], TypeMetadata.I32)
        );

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = pointType },
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
                        new Type(null, "Test") { Metadata = testType }
                    )
                    {
                        Metadata = tParameter,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): void {
                p.x = 1;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var point = new TypeMetadata("Point");
        point.AddConstructor(
            new ConstructorMetadata(
                point,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], point)));

        var xProperty = new PropertyMetadata(
            point,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.Void));

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new Type(null, "void") { Metadata = TypeMetadata.Void },
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
                                    ReturnTypeMetadata = TypeMetadata.I32,
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
            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): i32 {
                return p.x = 1;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var point = new TypeMetadata("Point");
        point.AddConstructor(
            new ConstructorMetadata(
                point,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], point)));

        var xProperty = new PropertyMetadata(
            point,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var tmpVariable = new VariableMetadata("<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ReturnStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                tmpVariable.Name,
                                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
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
            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): void {
                p.x += 1;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var point = new TypeMetadata("Point");
        point.AddConstructor(
            new ConstructorMetadata(
                point,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], point)));

        var xProperty = new PropertyMetadata(
            point,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.Void));

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new Type(null, "void") { Metadata = TypeMetadata.Void },
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
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
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
            public type Point {
                x: i32 { public get; public set; }
            }

            public test(p: Point): i32 {
                return p.x += 1;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var point = new TypeMetadata("Point");
        point.AddConstructor(
            new ConstructorMetadata(
                point,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], point)));

        var xProperty = new PropertyMetadata(
            point,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        point.AddProperty(xProperty);
        point.AddMethod(xProperty.Getter!);
        point.AddMethod(xProperty.Setter!);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var tmpVariable = new VariableMetadata("<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SemanticTree(null, [
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
                        new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                        new Type(null, "Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ReturnStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                tmpVariable.Name,
                                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
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