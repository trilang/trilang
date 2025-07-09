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
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new BinaryInstruction(new Register(2), @operator, new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
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
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new BinaryInstruction(new Register(2), @operator, new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 1),
                new MoveInstruction(new Register(2), new Register(1)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), @operator, new Register(0), new Register(1)),
                new MoveInstruction(new Register(3), new Register(2)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), Add, new Register(0), new Register(1)),
                new MoveInstruction(new Register(4), new Register(2)),
                new LoadInstruction(new Register(3), 10),
                new MoveInstruction(new Register(5), new Register(3)),
                new ReturnInstruction(new Register(5)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), Add, new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
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
                new LoadInstruction(new Register(0), 1),
                new MoveInstruction(new Register(1), new Register(0)),
                new LoadInstruction(new Register(2), 2),
                new MoveInstruction(new Register(3), new Register(2)),
                new BinaryInstruction(new Register(4), Add, new Register(1), new Register(3)),
                new ReturnInstruction(new Register(4)),
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
                new LoadInstruction(new Register(0), null),
                new ReturnInstruction(new Register(0)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Neg, new Register(0)),
                new ReturnInstruction(new Register(1)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Not, new Register(0)),
                new ReturnInstruction(new Register(1)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Not, new Register(0)),
                new ReturnInstruction(new Register(1)),
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

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 0),
                new ArrayElement(new Register(2), new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
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

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new LoadInstruction(new Register(2), 2),
                new BinaryInstruction(new Register(3), Add, new Register(1), new Register(2)),
                new ArrayElement(new Register(4), new Register(0), new Register(3)),
                new ReturnInstruction(new Register(4)),
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

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 10),
                new NewArrayInstruction(new Register(1), new TypeArrayMetadata(TypeMetadata.I32), new Register(0)),
                new MoveInstruction(new Register(2), new Register(1)),
                new ReturnInstruction(new Register(2)),
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

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new LoadParameterInstruction(new Register(2), 2),
            ])),
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new NewObjectInstruction(new Register(2), ctor, [new Register(0), new Register(1)]),
                new ReturnInstruction(new Register(2)),
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
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), Ge, new Register(0), new Register(1)),
                new BranchInstruction(new Register(2), "if_0_then", "if_0_else"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new ReturnInstruction(new Register(0)),
                        new JumpInstruction("if_0_end"),
                    ],
                    [endBlock]
                ),
                new Block(
                    "if_0_else",
                    [
                        new ReturnInstruction(new Register(1)),
                        new JumpInstruction("if_0_end"),
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
            new PhiInstruction(new Register(7), [new Register(2), new Register(6)]),
            new ReturnInstruction(new Register(7)),
        ]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 0),
                new MoveInstruction(new Register(2), new Register(1)),
                new LoadInstruction(new Register(3), 0),
                new BinaryInstruction(new Register(4), Gt, new Register(0), new Register(3)),
                new BranchInstruction(new Register(4), "if_0_then", "if_0_end"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new LoadInstruction(new Register(5), 10),
                        new MoveInstruction(new Register(6), new Register(5)),
                        new JumpInstruction("if_0_end"),
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
        var endBlock1 = new Block("if_1_end", [new JumpInstruction("if_0_end")], [endBlock0]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 0),
                new BinaryInstruction(new Register(2), Gt, new Register(0), new Register(1)),
                new BranchInstruction(new Register(2), "if_0_then", "if_0_else"),
            ],
            [
                new Block(
                    "if_0_then",
                    [
                        new LoadInstruction(new Register(3), 10),
                        new BinaryInstruction(new Register(4), Gt, new Register(0), new Register(3)),
                        new BranchInstruction(new Register(4), "if_1_then", "if_1_else"),
                    ],
                    [
                        new Block(
                            "if_1_then",
                            [
                                new LoadInstruction(new Register(5), 1),
                                new ReturnInstruction(new Register(5)),
                                new JumpInstruction("if_1_end"),
                            ],
                            [endBlock1]
                        ),
                        new Block(
                            "if_1_else",
                            [
                                new LoadInstruction(new Register(6), 2),
                                new ReturnInstruction(new Register(6)),
                                new JumpInstruction("if_1_end"),
                            ],
                            [endBlock1]
                        ),
                    ]
                ),
                new Block(
                    "if_0_else",
                    [
                        new LoadInstruction(new Register(7), 0),
                        new ReturnInstruction(new Register(7)),
                        new JumpInstruction("if_0_end"),
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
            new PhiInstruction(new Register(10), [new Register(8), new Register(9)]),
            new ReturnInstruction(new Register(10))
        ]);
        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block(
                "entry",
                [
                    new LoadParameterInstruction(new Register(0), 0),
                    new LoadInstruction(new Register(1), 0),
                    new MoveInstruction(new Register(2), new Register(1)),
                    new LoadInstruction(new Register(3), 0),
                    new BinaryInstruction(new Register(4), Gt, new Register(0), new Register(3)),
                    new BranchInstruction(new Register(4), "if_0_then", "if_0_else"),
                ],
                [
                    new Block(
                        "if_0_then",
                        [
                            new LoadInstruction(new Register(5), 1),
                            new MoveInstruction(new Register(9), new Register(5)),
                            new JumpInstruction("if_0_end"),
                        ],
                        [endBlock]
                    ),
                    new Block(
                        "if_0_else",
                        [
                            new LoadInstruction(new Register(6), 1),
                            new UnaryInstruction(new Register(7), UnaryInstructionKind.Neg, new Register(6)),
                            new MoveInstruction(new Register(8), new Register(7)),
                            new JumpInstruction("if_0_end"),
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
                new ReturnInstruction(new Register(6))
            ]
        );
        var ifThen = new Block(
            "if_0_then",
            [
                new LoadInstruction(new Register(4), 1),
                new BinaryInstruction(new Register(5), Add, new Register(6), new Register(4)),
                new MoveInstruction(new Register(7), new Register(5)),
                new JumpInstruction("loop_0_start"),
            ]
        );
        var loopStart = new Block(
            "loop_0_start",
            [
                new PhiInstruction(new Register(6), [new Register(1), new Register(7)]),
                new LoadInstruction(new Register(2), 10),
                new BinaryInstruction(new Register(3), Lt, new Register(6), new Register(2)),
                new BranchInstruction(new Register(3), "if_0_then", "loop_0_end"),
            ],
            [ifThen, loopEnd]
        );
        ifThen.AddNext(loopStart);
        var entry = new Block(
            "entry",
            [
                new LoadInstruction(new Register(0), 0),
                new MoveInstruction(new Register(1), new Register(0)),
                new JumpInstruction("loop_0_start"),
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