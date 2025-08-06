using Trilang.IntermediateRepresentation;
using Trilang.IntermediateRepresentation.Instructions;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using static Trilang.IntermediateRepresentation.Instructions.BinaryInstructionKind;

namespace Tri.Tests.IntermediateRepresentation;

public class IrGeneratorTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        return tree;
    }

    [Test]
    [TestCase("+", Add)]
    [TestCase("-", Sub)]
    [TestCase("*", Mul)]
    [TestCase("/", Div)]
    [TestCase("%", Mod)]
    [TestCase("&", And)]
    [TestCase("|", Or)]
    [TestCase("^", Xor)]
    public void TwoConstantsNumericOperatorsTest(string op, BinaryInstructionKind @operator)
    {
        var code =
            $$"""
              function test(): i32 {
                  return 1 {{op}} 2;
              }
              """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 1),
                new LoadConst(new Register(1, TypeMetadata.I32), 2),
                new BinaryOperation(
                    new Register(2, TypeMetadata.I32),
                    @operator,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Return(new Register(2, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    [TestCase("==", Eq)]
    [TestCase("!=", Ne)]
    [TestCase("<", Lt)]
    [TestCase("<=", Le)]
    [TestCase(">", Gt)]
    [TestCase(">=", Ge)]
    public void TwoConstantsEqualityOperatorsTest(string op, BinaryInstructionKind @operator)
    {
        var code =
            $$"""
              function test(): bool {
                  return 1 {{op}} 2;
              }
              """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 1),
                new LoadConst(new Register(1, TypeMetadata.I32), 2),
                new BinaryOperation(
                    new Register(2, TypeMetadata.Bool),
                    @operator,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Return(new Register(2, TypeMetadata.Bool)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void AssigmentOperatorTests()
    {
        const string code =
            """
            function test(x: i32): void {
                x = 1;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 1),
                new Move(new Register(2, TypeMetadata.I32), new Register(1, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    [TestCase("+=", Add)]
    [TestCase("-=", Sub)]
    [TestCase("*=", Mul)]
    [TestCase("/=", Div)]
    [TestCase("%=", Mod)]
    [TestCase("&=", And)]
    [TestCase("|=", Or)]
    [TestCase("^=", Xor)]
    public void NumericAssigmentOperatorTests(string op, BinaryInstructionKind @operator)
    {
        var code =
            $$"""
              function test(x: i32): void {
                  x {{op}} 1;
              }
              """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 1),
                new BinaryOperation(
                    new Register(2, TypeMetadata.I32),
                    @operator,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Move(new Register(3, TypeMetadata.I32), new Register(2, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void AssignmentSsaFormTest()
    {
        const string code =
            """
            function test(x: i32): i32 {
                x += 1;
                x = 10;

                return x;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 1),
                new BinaryOperation(
                    new Register(2, TypeMetadata.I32),
                    Add,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Move(new Register(4, TypeMetadata.I32), new Register(2, TypeMetadata.I32)),
                new LoadConst(new Register(3, TypeMetadata.I32), 10),
                new Move(new Register(5, TypeMetadata.I32), new Register(3, TypeMetadata.I32)),
                new Return(new Register(5, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void AddTwoParametersTest()
    {
        const string code =
            """
            function test(a: i32, b: i32): i32 {
                return a + b;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new BinaryOperation(
                    new Register(2, TypeMetadata.I32),
                    Add,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Return(new Register(2, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void AddTwoVariablesTest()
    {
        const string code =
            """
            function test(): i32 {
                var a: i32 = 1;
                var b: i32 = 2;

                return a + b;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 1),
                new Move(new Register(1, TypeMetadata.I32), new Register(0, TypeMetadata.I32)),
                new LoadConst(new Register(2, TypeMetadata.I32), 2),
                new Move(new Register(3, TypeMetadata.I32), new Register(2, TypeMetadata.I32)),
                new BinaryOperation(
                    new Register(4, TypeMetadata.I32),
                    Add,
                    new Register(1, TypeMetadata.I32),
                    new Register(3, TypeMetadata.I32)
                ),
                new Return(new Register(4, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void NullExpressionTest()
    {
        const string code =
            """
            function test(): null {
                return null;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.Null), null),
                new Return(new Register(0, TypeMetadata.Null)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void UnaryMinusExpressionTest()
    {
        const string code =
            """
            function test(a: i32): i32 {
                return -a;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new UnaryOperation(
                    new Register(1, TypeMetadata.I32),
                    UnaryInstructionKind.Neg,
                    new Register(0, TypeMetadata.I32)
                ),
                new Return(new Register(1, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void LogicalNotExpressionTest()
    {
        const string code =
            """
            function test(a: bool): bool {
                return !a;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.Bool), 0),
                new UnaryOperation(
                    new Register(1, TypeMetadata.Bool),
                    UnaryInstructionKind.Not,
                    new Register(0, TypeMetadata.Bool)
                ),
                new Return(new Register(1, TypeMetadata.Bool)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void BitwiseNotExpressionTest()
    {
        const string code =
            """
            function test(a: i32): i32 {
                return ~a;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new UnaryOperation(
                    new Register(1, TypeMetadata.I32),
                    UnaryInstructionKind.Not,
                    new Register(0, TypeMetadata.I32)
                ),
                new Return(new Register(1, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void ArrayAccessTest()
    {
        const string code =
            """
            function test(a: i32[]): i32 {
                return a[0];
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new ArrayElement(
                    new Register(2, TypeMetadata.I32),
                    new Register(0, arrayPointerType),
                    new Register(1, TypeMetadata.I32)
                ),
                new Return(new Register(2, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void ArrayAccessWithExpressionTest()
    {
        const string code =
            """
            function test(a: i32[], index: i32): i32 {
                return a[index + 2];
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new LoadConst(new Register(2, TypeMetadata.I32), 2),
                new BinaryOperation(
                    new Register(3, TypeMetadata.I32),
                    Add,
                    new Register(1, TypeMetadata.I32),
                    new Register(2, TypeMetadata.I32)
                ),
                new ArrayElement(
                    new Register(4, TypeMetadata.I32),
                    new Register(0, arrayPointerType),
                    new Register(3, TypeMetadata.I32)
                ),
                new Return(new Register(4, TypeMetadata.I32)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CreateIntArrayTest()
    {
        const string code =
            """
            function test(): i32[] {
                var a: i32[] = new i32[10];

                return a;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 10),
                new NewArray(
                    new Register(1, arrayPointerType),
                    new TypeArrayMetadata(TypeMetadata.I32),
                    new Register(0, TypeMetadata.I32)
                ),
                new Move(new Register(2, arrayPointerType), new Register(1, arrayPointerType)),
                new Return(new Register(2, arrayPointerType)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CreateTypeTest()
    {
        const string code =
            """
            public type Point {
                public constructor(x: i32, y: i32) { }
            }

            function test(): Point {
                return new Point(1, 2);
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var ctor = pointType.Constructors.First();
        var pointPointerType = new TypePointerMetadata(pointType);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new LoadParameter(new Register(2, TypeMetadata.I32), 2),
            ])),
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 1),
                new LoadConst(new Register(1, TypeMetadata.I32), 2),
                new NewObject(
                    new Register(2, pointPointerType),
                    ctor,
                    [new Register(0, TypeMetadata.I32), new Register(1, TypeMetadata.I32)]
                ),
                new Return(new Register(2, pointPointerType)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void IfStatementTest()
    {
        const string code =
            """
            function max(a: i32, b: i32): i32 {
                if (a >= b) {
                    return a;
                } else {
                    return b;
                }
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var endBlock = new Block("if_0_end");
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new BinaryOperation(
                    new Register(2, TypeMetadata.Bool),
                    Ge,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Branch(new Register(2, TypeMetadata.Bool), "if_0_then", "if_0_else"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new Return(new Register(0, TypeMetadata.I32)),
                        new Jump("if_0_end"),
                    ],
                    [endBlock]
                ),
                new Block(
                    "if_0_else",
                    [
                        new Return(new Register(1, TypeMetadata.I32)),
                        new Jump("if_0_end"),
                    ],
                    [endBlock]
                ),
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void IfStatementWithoutElseTest()
    {
        const string code =
            """
            function max(a: i32): i32 {
                var b: i32 = 0;
                if (a > 0) {
                    b = 10;
                }

                return b;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var endBlock = new Block("if_0_end", [
            new Phi(
                new Register(7, TypeMetadata.I32),
                [new Register(2, TypeMetadata.I32), new Register(6, TypeMetadata.I32)]
            ),
            new Return(new Register(7, TypeMetadata.I32)),
        ]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new Move(new Register(2, TypeMetadata.I32), new Register(1, TypeMetadata.I32)),
                new LoadConst(new Register(3, TypeMetadata.I32), 0),
                new BinaryOperation(
                    new Register(4, TypeMetadata.Bool),
                    Gt,
                    new Register(0, TypeMetadata.I32),
                    new Register(3, TypeMetadata.I32)
                ),
                new Branch(new Register(4, TypeMetadata.Bool), "if_0_then", "if_0_end"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new LoadConst(new Register(5, TypeMetadata.I32), 10),
                        new Move(new Register(6, TypeMetadata.I32), new Register(5, TypeMetadata.I32)),
                        new Jump("if_0_end"),
                    ],
                    [endBlock]
                ),
                endBlock
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void NestedIfStatementsTest()
    {
        const string code =
            """
            function max(a: i32): i32 {
                if (a > 0) {
                    if (a > 10) {
                        return 1;
                    } else {
                        return 2;
                    }
                } else {
                    return 0;
                }
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var endBlock0 = new Block("if_0_end");
        var endBlock1 = new Block("if_1_end", [new Jump("if_0_end")], [endBlock0]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new BinaryOperation(
                    new Register(2, TypeMetadata.Bool),
                    Gt,
                    new Register(0, TypeMetadata.I32),
                    new Register(1, TypeMetadata.I32)
                ),
                new Branch(new Register(2, TypeMetadata.Bool), "if_0_then", "if_0_else"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new LoadConst(new Register(3, TypeMetadata.I32), 10),
                        new BinaryOperation(
                            new Register(4, TypeMetadata.Bool),
                            Gt,
                            new Register(0, TypeMetadata.I32),
                            new Register(3, TypeMetadata.I32)
                        ),
                        new Branch(new Register(4, TypeMetadata.Bool), "if_1_then", "if_1_else"),
                    ],
                    [
                        new Block(
                            "if_1_then",
                            [
                                new LoadConst(new Register(5, TypeMetadata.I32), 1),
                                new Return(new Register(5, TypeMetadata.I32)),
                                new Jump("if_1_end"),
                            ],
                            [endBlock1]
                        ),
                        new Block(
                            "if_1_else",
                            [
                                new LoadConst(new Register(6, TypeMetadata.I32), 2),
                                new Return(new Register(6, TypeMetadata.I32)),
                                new Jump("if_1_end"),
                            ],
                            [endBlock1]
                        ),
                    ]
                ),
                new Block(
                    "if_0_else",
                    [
                        new LoadConst(new Register(7, TypeMetadata.I32), 0),
                        new Return(new Register(7, TypeMetadata.I32)),
                        new Jump("if_0_end"),
                    ],
                    [endBlock0]
                ),
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void IfStatementPhiFunctionTest()
    {
        const string code =
            """
            function test(a: i32): i32 {
                var b: i32 = 0;
                if (a > 0) {
                    b = 1;
                } else {
                    b = -1;
                }

                return b;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var endBlock = new Block("if_0_end", [
            new Phi(
                new Register(10, TypeMetadata.I32),
                [new Register(8, TypeMetadata.I32), new Register(9, TypeMetadata.I32)]
            ),
            new Return(new Register(10, TypeMetadata.I32))
        ]);
        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block(
                "entry",
                [
                    new LoadParameter(new Register(0, TypeMetadata.I32), 0),
                    new LoadConst(new Register(1, TypeMetadata.I32), 0),
                    new Move(new Register(2, TypeMetadata.I32), new Register(1, TypeMetadata.I32)),
                    new LoadConst(new Register(3, TypeMetadata.I32), 0),
                    new BinaryOperation(
                        new Register(4, TypeMetadata.Bool),
                        Gt,
                        new Register(0, TypeMetadata.I32),
                        new Register(3, TypeMetadata.I32)
                    ),
                    new Branch(new Register(4, TypeMetadata.Bool), "if_0_then", "if_0_else"),
                ],
                [
                    new Block(
                        "if_0_then",
                        [
                            new LoadConst(new Register(5, TypeMetadata.I32), 1),
                            new Move(new Register(9, TypeMetadata.I32), new Register(5, TypeMetadata.I32)),
                            new Jump("if_0_end"),
                        ],
                        [endBlock]
                    ),
                    new Block(
                        "if_0_else",
                        [
                            new LoadConst(new Register(6, TypeMetadata.I32), 1),
                            new UnaryOperation(
                                new Register(7, TypeMetadata.I32),
                                UnaryInstructionKind.Neg,
                                new Register(6, TypeMetadata.I32)
                            ),
                            new Move(new Register(8, TypeMetadata.I32), new Register(7, TypeMetadata.I32)),
                            new Jump("if_0_end"),
                        ],
                        [endBlock]
                    ),
                ]
            ))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void WhileStatementTest()
    {
        const string code =
            """
            function test(): i32 {
                var i: i32 = 0;
                while (i < 10) {
                    i += 1;
                }

                return i;
            }
            """;
        var tree = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate([tree]);

        var loopEnd = new Block(
            "loop_0_end", [
                new Return(new Register(6, TypeMetadata.I32))
            ]
        );
        var ifThen = new Block(
            "if_0_then",
            [
                new LoadConst(new Register(4, TypeMetadata.I32), 1),
                new BinaryOperation(
                    new Register(5, TypeMetadata.I32),
                    Add,
                    new Register(6, TypeMetadata.I32),
                    new Register(4, TypeMetadata.I32)
                ),
                new Move(new Register(7, TypeMetadata.I32), new Register(5, TypeMetadata.I32)),
                new Jump("loop_0_start"),
            ]
        );
        var loopStart = new Block(
            "loop_0_start",
            [
                new Phi(
                    new Register(6, TypeMetadata.I32),
                    [new Register(1, TypeMetadata.I32), new Register(7, TypeMetadata.I32)]
                ),
                new LoadConst(new Register(2, TypeMetadata.I32), 10),
                new BinaryOperation(
                    new Register(3, TypeMetadata.Bool),
                    Lt,
                    new Register(6, TypeMetadata.I32),
                    new Register(2, TypeMetadata.I32)
                ),
                new Branch(new Register(3, TypeMetadata.Bool), "if_0_then", "loop_0_end"),
            ],
            [ifThen, loopEnd]
        );
        ifThen.AddNext(loopStart);
        var entry = new Block(
            "entry",
            [
                new LoadConst(new Register(0, TypeMetadata.I32), 0),
                new Move(new Register(1, TypeMetadata.I32), new Register(0, TypeMetadata.I32)),
                new Jump("loop_0_start"),
            ],
            [loopStart]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("test", entry)
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }
}