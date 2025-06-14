using Trilang.IntermediateRepresentation;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using static Trilang.IntermediateRepresentation.BinaryInstructionKind;

namespace Tri.Tests.IntermediateRepresentation;

public class IrGeneratorTests
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
    public void AddTwoConstantsTest()
    {
        const string code =
            """
            function test(): i32 {
                return 1 + 2;
            }
            """;
        var tree = Parse(code);

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new BinaryInstruction(new Register(2), Add, new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
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

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new BinaryInstruction(new Register(2), Add, new Register(0), new Register(1)),
                new ReturnInstruction(new Register(2)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), null),
                new ReturnInstruction(new Register(0)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Neg, new Register(0)),
                new ReturnInstruction(new Register(1)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Not, new Register(0)),
                new ReturnInstruction(new Register(1)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new UnaryInstruction(new Register(1), UnaryInstructionKind.Not, new Register(0)),
                new ReturnInstruction(new Register(1)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
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

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
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

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 10),
                new NewArrayInstruction(new Register(1), new TypeArrayMetadata(TypeMetadata.I32), new Register(0)),
                new ReturnInstruction(new Register(1)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var ctor = pointType.Constructors.First();

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
            ])),
            new IrFunction("test", new Block("entry", [
                new LoadInstruction(new Register(0), 1),
                new LoadInstruction(new Register(1), 2),
                new NewObjectInstruction(new Register(2), ctor, [new Register(0), new Register(1)]),
                new ReturnInstruction(new Register(2)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var endBlock = new Block("endif_0");
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), Ge, new Register(0), new Register(1)),
                new BranchInstruction(new Register(2), "then_0", "else_0"),
            ],
            [
                new Block(
                    "then_0",
                    [new ReturnInstruction(new Register(0))],
                    [endBlock]
                ),
                new Block(
                    "else_0",
                    [new ReturnInstruction(new Register(1))],
                    [endBlock]
                ),
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected));
    }

    [Test]
    public void IfStatementWithoutElseTest()
    {
        const string code =
            """
            function max(a: i32, b: i32): i32 {
                if (a >= b) {
                    return a;
                }

                return 0;
            }
            """;
        var tree = Parse(code);

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var endBlock = new Block("endif_0", [
            new LoadInstruction(new Register(3), 0),
            new ReturnInstruction(new Register(3)),
        ]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadParameterInstruction(new Register(1), 1),
                new BinaryInstruction(new Register(2), Ge, new Register(0), new Register(1)),
                new BranchInstruction(new Register(2), "then_0", null),
            ],
            [
                new Block(
                    "then_0",
                    [new ReturnInstruction(new Register(0))],
                    [endBlock]
                ),
                endBlock
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected));
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

        var ir = new Ir();
        var functions = ir.Generate([tree]);

        var endBlock0 = new Block("endif_0");
        var endBlock1 = new Block("endif_1", [], [endBlock0]);
        var entryBlock = new Block(
            "entry",
            [
                new LoadParameterInstruction(new Register(0), 0),
                new LoadInstruction(new Register(1), 0),
                new BinaryInstruction(new Register(2), Gt, new Register(0), new Register(1)),
                new BranchInstruction(new Register(2), "then_0", "else_0"),
            ],
            [
                new Block(
                    "then_0",
                    [
                        new LoadInstruction(new Register(3), 10),
                        new BinaryInstruction(new Register(4), Gt, new Register(0), new Register(3)),
                        new BranchInstruction(new Register(4), "then_1", "else_1"),
                    ],
                    [
                        new Block(
                            "then_1",
                            [
                                new LoadInstruction(new Register(5), 1),
                                new ReturnInstruction(new Register(5)),
                            ],
                            [endBlock1]
                        ),
                        new Block(
                            "else_1",
                            [
                                new LoadInstruction(new Register(6), 2),
                                new ReturnInstruction(new Register(6)),
                            ],
                            [endBlock1]
                        ),
                    ]
                ),
                new Block(
                    "else_0",
                    [
                        new LoadInstruction(new Register(7), 0),
                        new ReturnInstruction(new Register(7)),
                    ],
                    [endBlock0]
                ),
            ]
        );

        var expected = new List<IrFunction>
        {
            new IrFunction("max", entryBlock)
        };

        Assert.That(functions, Is.EqualTo(expected));
    }
}