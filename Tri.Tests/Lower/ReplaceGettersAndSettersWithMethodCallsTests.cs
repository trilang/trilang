using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class ReplaceGettersAndSettersWithMethodCallsTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return tree;
    }

    [Test]
    public void ReplaceReadAccessWithGetterMethodCall()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32;
            }

            function test(p: Point): i32 {
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
        point.AddMethod(count.Getter);
        point.AddMethod(count.Setter);

        var backingField = new FieldMetadata(point, $"<>_{count.Name}", count.Type);
        point.AddField(backingField);

        var valueParameter = count.Setter.Parameters[0];

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(backingField.Name)
                                    {
                                        Reference = backingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = count.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(backingField.Name)
                                        {
                                            Reference = backingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(valueParameter.Name)
                                        {
                                            Reference = valueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = count.Setter,
                        }
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "p",
                        new TypeNode("Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("p")
                                {
                                    Reference = pParameter,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                count.Getter.Name
                            )
                            {
                                Reference = count.Getter,
                                AccessKind = PropertyAccessKind.Read,
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceNestedReadAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            public type Test {
                point: Point;
            }

            function test(t: Test): i32 {
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
        pointType.AddMethod(xProperty.Getter);
        pointType.AddMethod(xProperty.Setter);

        var xBackingField = new FieldMetadata(pointType, $"<>_{xProperty.Name}", xProperty.Type);
        pointType.AddField(xBackingField);

        var xValueParameter = xProperty.Setter.Parameters[0];

        var testType = new TypeMetadata("Test");
        testType.AddConstructor(
            new ConstructorMetadata(
                testType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], testType)));

        var pointProperty = new PropertyMetadata(testType, "point", pointType);
        testType.AddProperty(pointProperty);
        testType.AddMethod(pointProperty.Getter);
        testType.AddMethod(pointProperty.Setter);

        var pointBackingField = new FieldMetadata(testType, $"<>_{pointProperty.Name}", pointProperty.Type);
        testType.AddField(pointBackingField);

        var pointValueParameter = pointProperty.Setter.Parameters[0];

        var tParameter = new ParameterMetadata("t", testType);
        var testFunction = new FunctionMetadata(
            "test",
            [tParameter],
            new FunctionTypeMetadata([testType], TypeMetadata.I32)
        );

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(xBackingField.Name)
                                    {
                                        Reference = xBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = xProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(xBackingField.Name)
                                        {
                                            Reference = xBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(xValueParameter.Name)
                                        {
                                            Reference = xValueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = xProperty.Type,
                                    }
                                ),
                            ])
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
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Test",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "point",
                        new TypeNode("Point") { Metadata = pointType },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(pointBackingField.Name)
                                    {
                                        Reference = pointBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = pointProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Private,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(pointBackingField.Name)
                                        {
                                            Reference = pointBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(pointValueParameter.Name)
                                        {
                                            Reference = pointValueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = pointProperty.Type,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = pointProperty.Setter,
                        }
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "t",
                        new TypeNode("Test") { Metadata = testType }
                    )
                    {
                        Metadata = tParameter,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode(
                                new CallExpressionNode(
                                    new MemberAccessExpressionNode(
                                        new MemberAccessExpressionNode("t")
                                        {
                                            Reference = tParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        pointProperty.Getter.Name
                                    )
                                    {
                                        Reference = pointProperty.Getter,
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    []
                                ),
                                xProperty.Getter.Name
                            )
                            {
                                Reference = xProperty.Getter,
                                AccessKind = PropertyAccessKind.Read,
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceWriteAccessWithSetterMethodCall()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            function test(p: Point): void {
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
        point.AddMethod(xProperty.Getter);
        point.AddMethod(xProperty.Setter);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var valueParameter = xProperty.Setter.Parameters[0];

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.Void));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(xBackingField.Name)
                                    {
                                        Reference = xBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = xProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(xBackingField.Name)
                                        {
                                            Reference = xBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(valueParameter.Name)
                                        {
                                            Reference = valueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                ),
                            ])
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "p",
                        new TypeNode("Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new TypeNode("void") { Metadata = TypeMetadata.Void },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("p")
                                {
                                    Reference = pParameter,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                xProperty.Setter.Name
                            )
                            {
                                Reference = xProperty.Setter,
                                AccessKind = PropertyAccessKind.Read,
                            },
                            [
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceReadAndWriteAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            function test(p: Point): i32 {
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
        point.AddMethod(xProperty.Getter);
        point.AddMethod(xProperty.Setter);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var valueParameter = xProperty.Setter.Parameters[0];
        var tmpVariable = new VariableMetadata("<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(xBackingField.Name)
                                    {
                                        Reference = xBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = xProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(xBackingField.Name)
                                        {
                                            Reference = xBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(valueParameter.Name)
                                        {
                                            Reference = valueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                ),
                            ])
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "p",
                        new TypeNode("Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new ExpressionBlockNode([
                            new VariableDeclarationStatementNode(
                                tmpVariable.Name,
                                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
                                }
                            )
                            {
                                Metadata = tmpVariable,
                            },
                            new ExpressionStatementNode(
                                new CallExpressionNode(
                                    new MemberAccessExpressionNode(
                                        new MemberAccessExpressionNode("p")
                                        {
                                            Reference = pParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        xProperty.Setter.Name
                                    )
                                    {
                                        Reference = xProperty.Setter,
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    [
                                        new MemberAccessExpressionNode(tmpVariable.Name)
                                        {
                                            Reference = tmpVariable,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    ]
                                )
                            ),
                            new ExpressionStatementNode(
                                new MemberAccessExpressionNode(tmpVariable.Name)
                                {
                                    Reference = tmpVariable,
                                    AccessKind = PropertyAccessKind.Read,
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceCompoundAssignmentAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            function test(p: Point): void {
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
        point.AddMethod(xProperty.Getter);
        point.AddMethod(xProperty.Setter);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var valueParameter = xProperty.Setter.Parameters[0];

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.Void));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(xBackingField.Name)
                                    {
                                        Reference = xBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = xProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(xBackingField.Name)
                                        {
                                            Reference = xBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(valueParameter.Name)
                                        {
                                            Reference = valueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                ),
                            ])
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "p",
                        new TypeNode("Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new TypeNode("void") { Metadata = TypeMetadata.Void },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("p")
                                {
                                    Reference = pParameter,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                xProperty.Setter.Name
                            )
                            {
                                Reference = xProperty.Setter,
                                AccessKind = PropertyAccessKind.Read,
                            },
                            [
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Addition,
                                    new CallExpressionNode(
                                        new MemberAccessExpressionNode(
                                            new MemberAccessExpressionNode("p")
                                            {
                                                Reference = pParameter,
                                                AccessKind = PropertyAccessKind.Read,
                                            },
                                            xProperty.Getter.Name
                                        )
                                        {
                                            Reference = xProperty.Getter,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        []
                                    ),
                                    new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceReturnCompoundAssignmentAccessWithMethodCalls()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            function test(p: Point): i32 {
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
        point.AddMethod(xProperty.Getter);
        point.AddMethod(xProperty.Setter);

        var xBackingField = new FieldMetadata(point, $"<>_{xProperty.Name}", xProperty.Type);
        point.AddField(xBackingField);

        var valueParameter = xProperty.Setter.Parameters[0];
        var tmpVariable = new VariableMetadata("<>_tmp_set0", xProperty.Type);

        var pParameter = new ParameterMetadata("p", point);
        var testFunction = new FunctionMetadata(
            "test",
            [pParameter],
            new FunctionTypeMetadata([point], TypeMetadata.I32));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        "x",
                        new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                        new PropertyGetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ReturnStatementNode(
                                    new MemberAccessExpressionNode(xBackingField.Name)
                                    {
                                        Reference = xBackingField,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                ),
                            ])
                        )
                        {
                            Metadata = xProperty.Getter,
                        },
                        new PropertySetterNode(
                            AccessModifier.Public,
                            new BlockStatementNode([
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(xBackingField.Name)
                                        {
                                            Reference = xBackingField,
                                            AccessKind = PropertyAccessKind.Write,
                                        },
                                        new MemberAccessExpressionNode(valueParameter.Name)
                                        {
                                            Reference = valueParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32,
                                    }
                                ),
                            ])
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
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "p",
                        new TypeNode("Point") { Metadata = point }
                    )
                    {
                        Metadata = pParameter,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new ExpressionBlockNode([
                            new VariableDeclarationStatementNode(
                                tmpVariable.Name,
                                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Addition,
                                    new CallExpressionNode(
                                        new MemberAccessExpressionNode(
                                            new MemberAccessExpressionNode("p")
                                            {
                                                Reference = pParameter,
                                                AccessKind = PropertyAccessKind.Read,
                                            },
                                            xProperty.Getter.Name
                                        )
                                        {
                                            Reference = xProperty.Getter,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        []
                                    ),
                                    new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
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
                            new ExpressionStatementNode(
                                new CallExpressionNode(
                                    new MemberAccessExpressionNode(
                                        new MemberAccessExpressionNode("p")
                                        {
                                            Reference = pParameter,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        xProperty.Setter.Name
                                    )
                                    {
                                        Reference = xProperty.Setter,
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    [
                                        new MemberAccessExpressionNode(tmpVariable.Name)
                                        {
                                            Reference = tmpVariable,
                                            AccessKind = PropertyAccessKind.Read,
                                        }
                                    ]
                                )
                            ),
                            new ExpressionStatementNode(
                                new MemberAccessExpressionNode(tmpVariable.Name)
                                {
                                    Reference = tmpVariable,
                                    AccessKind = PropertyAccessKind.Read,
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

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}