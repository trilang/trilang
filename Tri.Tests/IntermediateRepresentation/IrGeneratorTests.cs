using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.IntermediateRepresentation;
using Trilang.IntermediateRepresentation.Instructions;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Trilang.IntermediateRepresentation.Instructions.BinaryInstructionKind;

namespace Tri.Tests.IntermediateRepresentation;

public class IrGeneratorTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SemanticTree, ITypeMetadataProvider) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();
        var lowering = new Lowering();
        lowering.Lower(semanticTree, LoweringOptions.Default);

        return (semanticTree, typeProvider);
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
              public test(): i32 {
                  return 1 {{op}} 2;
              }
              """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
              public test(): bool {
                  return 1 {{op}} 2;
              }
              """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(x: i32): void {
                x = 1;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
              public test(x: i32): void {
                  x {{op}} 1;
              }
              """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(x: i32): i32 {
                x += 1;
                x = 10;

                return x;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: i32, b: i32): i32 {
                return a + b;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(): i32 {
                var a: i32 = 1;
                var b: i32 = 2;

                return a + b;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(): null {
                return null;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: i32): i32 {
                return -a;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: bool): bool {
                return !a;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: i32): i32 {
                return ~a;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: i32[]): i32 {
                return a[0];
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);
        var arraySize = (FieldMetadata)arrayType.GetMember("<>_size")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new GetElementPointer(
                    new Register(2, new TypePointerMetadata(TypeMetadata.I32)),
                    new Register(0, arrayPointerType),
                    new Register(1, TypeMetadata.I32)
                ),
                new Load(
                    new Register(3, TypeMetadata.I32),
                    new Register(2, new TypePointerMetadata(TypeMetadata.I32))
                ),
                new Return(new Register(3, TypeMetadata.I32)),
            ])),
            new IrFunction("array_i32__<>_get_size", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new GetMemberPointer(
                    new Register(1, new TypePointerMetadata(arraySize.Type)),
                    new Register(0, arrayPointerType),
                    arraySize
                ),
                new Load(
                    new Register(2, arraySize.Type),
                    new Register(1, new TypePointerMetadata(arraySize.Type))
                ),
                new Return(new Register(2, arraySize.Type)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void ArrayAccessWithExpressionTest()
    {
        const string code =
            """
            public test(a: i32[], index: i32): i32 {
                return a[index + 2];
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);
        var arraySize = (FieldMetadata)arrayType.GetMember("<>_size")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
                new GetElementPointer(
                    new Register(4, new TypePointerMetadata(TypeMetadata.I32)),
                    new Register(0, arrayPointerType),
                    new Register(3, TypeMetadata.I32)
                ),
                new Load(
                    new Register(5, TypeMetadata.I32),
                    new Register(4, new TypePointerMetadata(TypeMetadata.I32))
                ),
                new Return(new Register(5, TypeMetadata.I32)),
            ])),
            new IrFunction("array_i32__<>_get_size", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new GetMemberPointer(
                    new Register(1, new TypePointerMetadata(arraySize.Type)),
                    new Register(0, arrayPointerType),
                    arraySize
                ),
                new Load(
                    new Register(2, arraySize.Type),
                    new Register(1, new TypePointerMetadata(arraySize.Type))
                ),
                new Return(new Register(2, arraySize.Type)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void ArraySetElementTest()
    {
        const string code =
            """
            public test(a: i32[]): void {
                a[0] = 10;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);
        var arraySize = (FieldMetadata)arrayType.GetMember("<>_size")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new GetElementPointer(
                    new Register(2, new TypePointerMetadata(TypeMetadata.I32)),
                    new Register(0, arrayPointerType),
                    new Register(1, TypeMetadata.I32)
                ),
                new LoadConst(new Register(3, TypeMetadata.I32), 10),
                new Store(
                    new Register(2, new TypePointerMetadata(TypeMetadata.I32)),
                    new Register(3, TypeMetadata.I32)
                ),
            ])),
            new IrFunction("array_i32__<>_get_size", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new GetMemberPointer(
                    new Register(1, new TypePointerMetadata(arraySize.Type)),
                    new Register(0, arrayPointerType),
                    arraySize
                ),
                new Load(
                    new Register(2, arraySize.Type),
                    new Register(1, new TypePointerMetadata(arraySize.Type))
                ),
                new Return(new Register(2, arraySize.Type)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CreateIntArrayTest()
    {
        const string code =
            """
            public test(): i32[] {
                var a: i32[] = new i32[10];

                return a;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var arrayType = (TypeArrayMetadata)typeProvider.GetType("i32[]")!;
        var arrayPointerType = new TypePointerMetadata(arrayType);
        var arraySize = (FieldMetadata)arrayType.GetMember("<>_size")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadConst(new Register(0, TypeMetadata.I32), 10),
                new ArrayAlloc(
                    new Register(1, arrayPointerType),
                    8,
                    4,
                    new Register(0, TypeMetadata.I32)
                ),
                new Move(new Register(2, arrayPointerType), new Register(1, arrayPointerType)),
                new Return(new Register(2, arrayPointerType)),
            ])),
            new IrFunction("array_i32__<>_get_size", new Block("entry", [
                new LoadParameter(new Register(0, arrayPointerType), 0),
                new GetMemberPointer(
                    new Register(1, new TypePointerMetadata(arraySize.Type)),
                    new Register(0, arrayPointerType),
                    arraySize
                ),
                new Load(
                    new Register(2, arraySize.Type),
                    new Register(1, new TypePointerMetadata(arraySize.Type))
                ),
                new Return(new Register(2, arraySize.Type)),
            ])),
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

            public test(): Point {
                return new Point(1, 2);
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var pointPointerType = new TypePointerMetadata(pointType);
        var ctor = pointType.Constructors.First();
        var ctorPointer = new TypePointerMetadata(ctor.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new LoadParameter(new Register(2, TypeMetadata.I32), 2),
            ])),
            new IrFunction("test", new Block("entry", [
                new Alloc(new Register(0, pointPointerType), 0),
                new GetMemberPointer(
                    new Register(1, ctorPointer),
                    new Register(0, pointPointerType),
                    ctor
                ),
                new Load(new Register(2, ctor.Type), new Register(1, ctorPointer)),
                new LoadConst(new Register(3, TypeMetadata.I32), 1),
                new LoadConst(new Register(4, TypeMetadata.I32), 2),
                new Call(
                    new Register(5, pointPointerType),
                    new Register(2, ctor.Type),
                    [
                        new Register(3, TypeMetadata.I32),
                        new Register(4, TypeMetadata.I32)
                    ],
                    false
                ),
                new Return(new Register(5, pointPointerType)),
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void IfStatementTest()
    {
        const string code =
            """
            public max(a: i32, b: i32): i32 {
                if (a >= b) {
                    return a;
                } else {
                    return b;
                }
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public max(a: i32): i32 {
                var b: i32 = 0;
                if (a > 0) {
                    b = 10;
                }

                return b;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public max(a: i32): i32 {
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
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(a: i32): i32 {
                var b: i32 = 0;
                if (a > 0) {
                    b = 1;
                } else {
                    b = -1;
                }

                return b;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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
            public test(): i32 {
                var i: i32 = 0;
                while (i < 10) {
                    i += 1;
                }

                return i;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

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

    [Test]
    public void CallMethodTest()
    {
        const string code =
            """
            public type Test {
                public method1(): i32 {
                    return 0;
                }

                public method2(): void {
                    this.method1();
                }
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var testType = (TypeMetadata)typeProvider.GetType("Test")!;
        var typePointer = new TypePointerMetadata(testType);
        var method = testType.GetMethod("method1")!;
        var methodPointer = new TypePointerMetadata(method.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Test_method1", new Block("entry", [
                new LoadParameter(new Register(0, new TypePointerMetadata(testType)), 0),
                new LoadConst(new Register(1, TypeMetadata.I32), 0),
                new Return(new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Test_method2", new Block("entry", [
                new LoadParameter(new Register(0, typePointer), 0),
                new GetMemberPointer(
                    new Register(1, methodPointer),
                    new Register(0, typePointer),
                    method
                ),
                new Load(new Register(2, method.Type), new Register(1, methodPointer)),
                new Call(
                    new Register(3, TypeMetadata.I32),
                    new Register(2, method.Type),
                    [],
                    false
                ),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallMethodWithVoidReturnTypeTest()
    {
        const string code =
            """
            public type Test {
                public method1(): void { }

                public method2(): void {
                    this.method1();
                }
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var testType = (TypeMetadata)typeProvider.GetType("Test")!;
        var typePointer = new TypePointerMetadata(testType);
        var method = testType.GetMethod("method1")!;
        var methodPointer = new TypePointerMetadata(method.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Test_method1", new Block("entry", [
                new LoadParameter(new Register(0, new TypePointerMetadata(testType)), 0),
            ])),
            new IrFunction("Test_method2", new Block("entry", [
                new LoadParameter(new Register(0, typePointer), 0),
                new GetMemberPointer(
                    new Register(1, methodPointer),
                    new Register(0, typePointer),
                    method
                ),
                new Load(new Register(2, method.Type), new Register(1, methodPointer)),
                new Call(
                    new Register(3, TypeMetadata.Void),
                    new Register(2, method.Type),
                    [],
                    false
                ),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallSetterTest()
    {
        const string code =
            """
            public type Point {
                public constructor(x: i32) {
                    this.x = x;
                }

                x: i32 {
                    public get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var pointPointerType = new TypePointerMetadata(pointType);
        var field = pointType.GetField("<>_x")!;
        var fieldPointer = new TypePointerMetadata(field.Type);
        var setter = pointType.GetMethod("<>_set_x")!;
        var setterPointer = new TypePointerMetadata(setter.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_<>_get_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new GetMemberPointer(new Register(1, fieldPointer), new Register(0, pointPointerType), field),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, fieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_set_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, fieldPointer), new Register(0, pointPointerType), field),
                new Store(new Register(2, fieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, setterPointer), new Register(0, pointPointerType), setter),
                new Load(new Register(3, setter.Type), new Register(2, setterPointer)),
                new Call(
                    new Register(4, TypeMetadata.Void),
                    new Register(3, setter.Type),
                    [
                        new Register(1, TypeMetadata.I32)
                    ],
                    false
                ),
            ])),
        };

        Assert.That(functions, Is.EquivalentTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallGetterTest()
    {
        const string code =
            """
            public type Point {
                public constructor(x: i32) {
                    this.x = x;
                }

                x: i32;
            }

            public test(): i32 {
                var p: Point = new Point(1);

                return p.x;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var pointPointerType = new TypePointerMetadata(pointType);
        var field = pointType.GetField("<>_x")!;
        var fieldPointer = new TypePointerMetadata(field.Type);
        var getter = pointType.GetMethod("<>_get_x")!;
        var getterPointer = new TypePointerMetadata(getter.Type);
        var setter = pointType.GetMethod("<>_set_x")!;
        var setterPointer = new TypePointerMetadata(setter.Type);
        var ctor = pointType.GetConstructor([TypeMetadata.I32])!;
        var ctorPointer = new TypePointerMetadata(ctor.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_<>_get_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new GetMemberPointer(new Register(1, fieldPointer), new Register(0, pointPointerType), field),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, fieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_set_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, fieldPointer), new Register(0, pointPointerType), field),
                new Store(new Register(2, fieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(
                    new Register(2, setterPointer),
                    new Register(0, pointPointerType),
                    setter
                ),
                new Load(new Register(3, setter.Type), new Register(2, setterPointer)),
                new Call(
                    new Register(4, TypeMetadata.Void),
                    new Register(3, setter.Type),
                    [
                        new Register(1, TypeMetadata.I32)
                    ],
                    false
                ),
            ])),
            new IrFunction("test", new Block("entry", [
                new Alloc(new Register(0, pointPointerType), 4),
                new GetMemberPointer(
                    new Register(1, ctorPointer),
                    new Register(0, pointPointerType),
                    ctor
                ),
                new Load(new Register(2, ctor.Type), new Register(1, ctorPointer)),
                new LoadConst(new Register(3, TypeMetadata.I32), 1),
                new Call(
                    new Register(4, pointPointerType),
                    new Register(2, ctor.Type),
                    [
                        new Register(3, TypeMetadata.I32)
                    ],
                    false
                ),
                new Move(new Register(5, pointPointerType), new Register(4, pointPointerType)),
                new GetMemberPointer(
                    new Register(6, getterPointer),
                    new Register(5, pointPointerType),
                    getter
                ),
                new Load(new Register(7, getter.Type), new Register(6, getterPointer)),
                new Call(
                    new Register(8, TypeMetadata.I32),
                    new Register(7, getter.Type),
                    [],
                    false
                ),
                new Return(new Register(8, TypeMetadata.I32)),
            ])),
        };

        Assert.That(functions, Is.EquivalentTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallUnaryOpTest()
    {
        const string code =
            """
            public type Point {
                public constructor(x: i32) {
                    this.x = x;
                }

                x: i32;
            }

            public test(): i32 {
                var p: Point = new Point(1);

                return -p.x;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var pointPointerType = new TypePointerMetadata(pointType);
        var field = pointType.GetField("<>_x")!;
        var fieldPointer = new TypePointerMetadata(field.Type);
        var getter = pointType.GetMethod("<>_get_x")!;
        var getterPointer = new TypePointerMetadata(getter.Type);
        var setter = pointType.GetMethod("<>_set_x")!;
        var setterPointer = new TypePointerMetadata(setter.Type);
        var ctor = pointType.GetConstructor([TypeMetadata.I32])!;
        var ctorPointer = new TypePointerMetadata(ctor.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_<>_get_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new GetMemberPointer(new Register(1, fieldPointer), new Register(0, pointPointerType), field),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, fieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_set_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, fieldPointer), new Register(0, pointPointerType), field),
                new Store(new Register(2, fieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, setterPointer), new Register(0, pointPointerType), setter),
                new Load(new Register(3, setter.Type), new Register(2, setterPointer)),
                new Call(
                    new Register(4, TypeMetadata.Void),
                    new Register(3, setter.Type),
                    [
                        new Register(1, TypeMetadata.I32)
                    ],
                    false
                ),
            ])),
            new IrFunction("test", new Block("entry", [
                new Alloc(new Register(0, pointPointerType), 4),
                new GetMemberPointer(new Register(1, ctorPointer), new Register(0, pointPointerType), ctor),
                new Load(new Register(2, ctor.Type), new Register(1, ctorPointer)),
                new LoadConst(new Register(3, TypeMetadata.I32), 1),
                new Call(
                    new Register(4, pointPointerType),
                    new Register(2, ctor.Type),
                    [
                        new Register(3, TypeMetadata.I32)
                    ],
                    false
                ),
                new Move(new Register(5, pointPointerType), new Register(4, pointPointerType)),
                new GetMemberPointer(new Register(6, getterPointer), new Register(5, pointPointerType), getter),
                new Load(new Register(7, getter.Type), new Register(6, getterPointer)),
                new Call(
                    new Register(8, TypeMetadata.I32),
                    new Register(7, getter.Type),
                    [],
                    false
                ),
                new UnaryOperation(
                    new Register(9, TypeMetadata.I32),
                    UnaryInstructionKind.Neg,
                    new Register(8, TypeMetadata.I32)
                ),
                new Return(new Register(9, TypeMetadata.I32)),
            ])),
        };

        Assert.That(functions, Is.EquivalentTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallBinaryOpTest()
    {
        const string code =
            """
            public type Point {
                public constructor(x: i32, y: i32) {
                    this.x = x;
                    this.y = y;
                }

                x: i32;
                y: i32;
            }

            public test(): i32 {
                var p: Point = new Point(1, 2);

                return p.x + p.y;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var pointType = (TypeMetadata)typeProvider.GetType("Point")!;
        var pointPointerType = new TypePointerMetadata(pointType);
        var xField = pointType.GetField("<>_x")!;
        var xFieldPointer = new TypePointerMetadata(xField.Type);
        var yField = pointType.GetField("<>_y")!;
        var yFieldPointer = new TypePointerMetadata(yField.Type);
        var xGetter = pointType.GetMethod("<>_get_x")!;
        var xGetterPointer = new TypePointerMetadata(xGetter.Type);
        var xSetter = pointType.GetMethod("<>_set_x")!;
        var xSetterPointer = new TypePointerMetadata(xSetter.Type);
        var yGetter = pointType.GetMethod("<>_get_y")!;
        var yGetterPointer = new TypePointerMetadata(yGetter.Type);
        var ySetter = pointType.GetMethod("<>_set_y")!;
        var ySetterPointer = new TypePointerMetadata(ySetter.Type);
        var ctor = pointType.GetConstructor([TypeMetadata.I32, TypeMetadata.I32])!;
        var ctorPointer = new TypePointerMetadata(ctor.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Point_<>_get_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new GetMemberPointer(new Register(1, xFieldPointer), new Register(0, pointPointerType), xField),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, xFieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_set_x", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, xFieldPointer), new Register(0, pointPointerType), xField),
                new Store(new Register(2, xFieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_get_y", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new GetMemberPointer(new Register(1, yFieldPointer), new Register(0, pointPointerType), yField),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, yFieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_<>_set_y", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(new Register(2, yFieldPointer), new Register(0, pointPointerType), yField),
                new Store(new Register(2, yFieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Point_ctor", new Block("entry", [
                new LoadParameter(new Register(0, pointPointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new LoadParameter(new Register(2, TypeMetadata.I32), 2),
                new GetMemberPointer(new Register(3, xSetterPointer), new Register(0, pointPointerType), xSetter),
                new Load(new Register(4, xSetter.Type), new Register(3, xSetterPointer)),
                new Call(
                    new Register(5, TypeMetadata.Void),
                    new Register(4, xSetter.Type),
                    [
                        new Register(1, TypeMetadata.I32)
                    ],
                    false
                ),
                new GetMemberPointer(new Register(6, ySetterPointer), new Register(0, pointPointerType), ySetter),
                new Load(new Register(7, ySetter.Type), new Register(6, ySetterPointer)),
                new Call(
                    new Register(8, TypeMetadata.Void),
                    new Register(7, ySetter.Type),
                    [
                        new Register(2, TypeMetadata.I32)
                    ],
                    false
                ),
            ])),
            new IrFunction("test", new Block("entry", [
                new Alloc(new Register(0, pointPointerType), 8),
                new GetMemberPointer(new Register(1, ctorPointer), new Register(0, pointPointerType), ctor),
                new Load(new Register(2, ctor.Type), new Register(1, ctorPointer)),
                new LoadConst(new Register(3, TypeMetadata.I32), 1),
                new LoadConst(new Register(4, TypeMetadata.I32), 2),
                new Call(
                    new Register(5, pointPointerType),
                    new Register(2, ctor.Type),
                    [
                        new Register(3, TypeMetadata.I32),
                        new Register(4, TypeMetadata.I32),
                    ],
                    false
                ),
                new Move(new Register(6, pointPointerType), new Register(5, pointPointerType)),
                new GetMemberPointer(
                    new Register(7, xGetterPointer),
                    new Register(6, pointPointerType),
                    xGetter
                ),
                new Load(new Register(8, xGetter.Type), new Register(7, xGetterPointer)),
                new Call(
                    new Register(9, TypeMetadata.I32),
                    new Register(8, xGetter.Type),
                    [],
                    false
                ),
                new GetMemberPointer(
                    new Register(10, yGetterPointer),
                    new Register(6, pointPointerType),
                    yGetter
                ),
                new Load(new Register(11, yGetter.Type), new Register(10, yGetterPointer)),
                new Call(
                    new Register(12, TypeMetadata.I32),
                    new Register(11, yGetter.Type),
                    [],
                    false
                ),
                new BinaryOperation(
                    new Register(13, TypeMetadata.I32),
                    Add,
                    new Register(9, TypeMetadata.I32),
                    new Register(12, TypeMetadata.I32)
                ),
                new Return(new Register(13, TypeMetadata.I32)),
            ])),
        };

        Assert.That(functions, Is.EquivalentTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallComplexTypeAccessTest()
    {
        const string code =
            """
            public type Test1 {
                x: i32;

                public constructor(x: i32) {
                    this.x = x;
                }
            }

            public type Test2 {
                obj: Test1;

                public constructor(obj: Test1) {
                    this.obj = obj;
                }
            }

            public test(): i32 {
                var test1: Test1 = new Test1(1);
                var test2: Test2 = new Test2(test1);

                return test2.obj.x;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var test1Type = (TypeMetadata)typeProvider.GetType("Test1")!;
        var test1PointerType = new TypePointerMetadata(test1Type);
        var test2Type = (TypeMetadata)typeProvider.GetType("Test2")!;
        var test2PointerType = new TypePointerMetadata(test2Type);

        // Test1 members
        var test1XField = test1Type.GetField("<>_x")!;
        var test1XFieldPointer = new TypePointerMetadata(test1XField.Type);
        var test1XGetter = test1Type.GetMethod("<>_get_x")!;
        var test1XGetterPointer = new TypePointerMetadata(test1XGetter.Type);
        var test1XSetter = test1Type.GetMethod("<>_set_x")!;
        var test1XSetterPointer = new TypePointerMetadata(test1XSetter.Type);
        var test1Ctor = test1Type.GetConstructor([TypeMetadata.I32])!;
        var test1CtorPointer = new TypePointerMetadata(test1Ctor.Type);

        // Test2 members
        var test2ObjField = test2Type.GetField("<>_obj")!;
        var test2ObjFieldPointer = new TypePointerMetadata(test2ObjField.Type);
        var test2ObjGetter = test2Type.GetMethod("<>_get_obj")!;
        var test2ObjGetterPointer = new TypePointerMetadata(test2ObjGetter.Type);
        var test2ObjSetter = test2Type.GetMethod("<>_set_obj")!;
        var test2ObjSetterPointer = new TypePointerMetadata(test2ObjSetter.Type);
        var test2Ctor = test2Type.GetConstructor([test1Type])!;
        var test2CtorPointer = new TypePointerMetadata(test2Ctor.Type);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            // Test1
            new IrFunction("Test1_<>_get_x", new Block("entry", [
                new LoadParameter(new Register(0, test1PointerType), 0),
                new GetMemberPointer(
                    new Register(1, test1XFieldPointer),
                    new Register(0, test1PointerType),
                    test1XField
                ),
                new Load(new Register(2, TypeMetadata.I32), new Register(1, test1XFieldPointer)),
                new Return(new Register(2, TypeMetadata.I32)),
            ])),
            new IrFunction("Test1_<>_set_x", new Block("entry", [
                new LoadParameter(new Register(0, test1PointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(
                    new Register(2, test1XFieldPointer),
                    new Register(0, test1PointerType),
                    test1XField
                ),
                new Store(new Register(2, test1XFieldPointer), new Register(1, TypeMetadata.I32)),
            ])),
            new IrFunction("Test1_ctor", new Block("entry", [
                new LoadParameter(new Register(0, test1PointerType), 0),
                new LoadParameter(new Register(1, TypeMetadata.I32), 1),
                new GetMemberPointer(
                    new Register(2, test1XSetterPointer),
                    new Register(0, test1PointerType),
                    test1XSetter
                ),
                new Load(new Register(3, test1XSetter.Type), new Register(2, test1XSetterPointer)),
                new Call(
                    new Register(4, TypeMetadata.Void),
                    new Register(3, test1XSetter.Type),
                    [
                        new Register(1, TypeMetadata.I32)
                    ],
                    false
                ),
            ])),

            // Test2
            new IrFunction("Test2_<>_get_obj", new Block("entry", [
                new LoadParameter(new Register(0, test2PointerType), 0),
                new GetMemberPointer(
                    new Register(1, new TypePointerMetadata(test2ObjFieldPointer)),
                    new Register(0, test2PointerType),
                    test2ObjField
                ),
                new Load(
                    new Register(2, test2ObjFieldPointer),
                    new Register(1, new TypePointerMetadata(test2ObjFieldPointer))
                ),
                new Return(new Register(2, test2ObjFieldPointer)),
            ])),
            new IrFunction("Test2_<>_set_obj", new Block("entry", [
                new LoadParameter(new Register(0, test2PointerType), 0),
                new LoadParameter(new Register(1, test1PointerType), 1),
                new GetMemberPointer(
                    new Register(2, new TypePointerMetadata(test2ObjFieldPointer)),
                    new Register(0, test2PointerType),
                    test2ObjField
                ),
                new Store(
                    new Register(2, new TypePointerMetadata(test2ObjFieldPointer)),
                    new Register(1, test1PointerType)
                ),
            ])),
            new IrFunction("Test2_ctor", new Block("entry", [
                new LoadParameter(new Register(0, test2PointerType), 0),
                new LoadParameter(new Register(1, test1PointerType), 1),
                new GetMemberPointer(
                    new Register(2, test2ObjSetterPointer),
                    new Register(0, test2PointerType),
                    test2ObjSetter
                ),
                new Load(new Register(3, test2ObjSetter.Type), new Register(2, test2ObjSetterPointer)),
                new Call(
                    new Register(4, TypeMetadata.Void),
                    new Register(3, test2ObjSetter.Type),
                    [
                        new Register(1, test1PointerType)
                    ],
                    false
                ),
            ])),

            // test
            new IrFunction("test", new Block("entry", [
                new Alloc(new Register(0, test1PointerType), 4),
                new GetMemberPointer(
                    new Register(1, test1CtorPointer),
                    new Register(0, test1PointerType),
                    test1Ctor
                ),
                new Load(new Register(2, test1Ctor.Type), new Register(1, test1CtorPointer)),
                new LoadConst(new Register(3, TypeMetadata.I32), 1),
                new Call(
                    new Register(4, test1PointerType),
                    new Register(2, test1Ctor.Type),
                    [
                        new Register(3, TypeMetadata.I32)
                    ],
                    false
                ),
                new Move(new Register(5, test1PointerType), new Register(4, test1PointerType)),

                new Alloc(new Register(6, test2PointerType), 8),
                new GetMemberPointer(
                    new Register(7, test2CtorPointer),
                    new Register(6, test2PointerType),
                    test2Ctor
                ),
                new Load(new Register(8, test2Ctor.Type), new Register(7, test2CtorPointer)),
                new Call(
                    new Register(9, test2PointerType),
                    new Register(8, test2Ctor.Type),
                    [
                        new Register(5, test1PointerType)
                    ],
                    false
                ),
                new Move(new Register(10, test2PointerType), new Register(9, test2PointerType)),

                new GetMemberPointer(
                    new Register(11, test2ObjGetterPointer),
                    new Register(10, test2PointerType),
                    test2ObjGetter
                ),
                new Load(new Register(12, test2ObjGetter.Type), new Register(11, test2ObjGetterPointer)),
                new Call(
                    new Register(13, test1PointerType),
                    new Register(12, test2ObjGetter.Type),
                    [],
                    false
                ),

                new GetMemberPointer(
                    new Register(14, test1XGetterPointer),
                    new Register(13, test1PointerType),
                    test1XGetter
                ),
                new Load(new Register(15, test1XGetter.Type), new Register(14, test1XGetterPointer)),
                new Call(
                    new Register(16, TypeMetadata.I32),
                    new Register(15, test1XGetter.Type),
                    [],
                    false
                ),

                new Return(new Register(16, TypeMetadata.I32)),
            ])),
        };

        Assert.That(functions, Is.EquivalentTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallFunctionTest()
    {
        const string code =
            """
            public main(): void {
                test1();
            }

            public test1(): void { }
            """;
        var (tree, typeProvider) = Parse(code);

        var functionType = (FunctionTypeMetadata)typeProvider.GetType("() => void")!;
        var functionPointer = new TypePointerMetadata(functionType);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("main", new Block("entry", [
                new GetMemberPointer(
                    new Register(0, functionPointer),
                    null,
                    new FunctionMetadata(null, AccessModifierMetadata.Public, "test1", [], functionType)
                ),
                new Load(new Register(1, functionType), new Register(0, functionPointer)),
                new Call(
                    new Register(2, TypeMetadata.Void),
                    new Register(1, functionType),
                    [],
                    true
                )
            ])),
            new IrFunction("test1", new Block("entry", [])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallStaticMethodTest()
    {
        const string code =
            """
            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var testType = (TypeMetadata)typeProvider.GetType("Test")!;
        var testMethod = testType.GetMethod("test")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("Test_test_s", new Block("entry", [])),
            new IrFunction("main", new Block("entry", [
                new GetMemberPointer(
                    new Register(0, new TypePointerMetadata(testMethod.Type)),
                    null,
                    testMethod
                ),
                new Load(
                    new Register(1, testMethod.Type),
                    new Register(0, new TypePointerMetadata(testMethod.Type))
                ),
                new Call(
                    new Register(2, TypeMetadata.Void),
                    new Register(1, testMethod.Type),
                    [],
                    true
                )
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallViaVariableTest()
    {
        const string code =
            """
            public test(): void {}

            public main(): void {
                var f: () => void = test;
                f();
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var functionType = (FunctionTypeMetadata)typeProvider.GetType("() => void")!;
        var functionPointer = new TypePointerMetadata(functionType);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [])),
            new IrFunction("main", new Block("entry", [
                new GetMemberPointer(
                    new Register(0, functionPointer),
                    null,
                    new FunctionMetadata(null, AccessModifierMetadata.Public, "test", [], functionType)
                ),
                new Load(new Register(1, functionType), new Register(0, functionPointer)),
                new Move(new Register(2, functionType), new Register(1, functionType)),
                new Call(
                    new Register(3, TypeMetadata.Void),
                    new Register(2, functionType),
                    [],
                    false
                )
            ]))
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallCallbackTest()
    {
        const string code =
            """
            public test(callback: () => void): void {
                callback();
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var functionType = (FunctionTypeMetadata)typeProvider.GetType("() => void")!;

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, functionType), 0),
                new Call(
                    new Register(1, TypeMetadata.Void),
                    new Register(0, functionType),
                    [],
                    false
                )
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CallInterfaceMethodTest()
    {
        const string code =
            """
            public type Interface = {
                method(): void;
            }

            public test(p: Interface): void {
                p.method();
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var interfaceType = (TypeAliasMetadata)typeProvider.GetType("Interface")!;
        var interfacePointer = new TypePointerMetadata(interfaceType);

        var method = (InterfaceMethodMetadata)interfaceType.GetMember("method")!;
        var methodPointer = new TypePointerMetadata(method.Type);

        var expected = new List<IrFunction>
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, interfacePointer), 0),
                new GetMemberPointer(
                    new Register(1, methodPointer),
                    new Register(0, interfacePointer),
                    method
                ),
                new Load(new Register(2, method.Type), new Register(1, methodPointer)),
                new Call(
                    new Register(3, TypeMetadata.Void),
                    new Register(2, method.Type),
                    [],
                    false
                ),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void IsTypeTest()
    {
        const string code =
            """
            public test(obj: {}): bool {
                return obj is i8;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var any = typeProvider.GetType("{ }")!;
        var anyPointer = new TypePointerMetadata(any);

        var expected = new[]
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, anyPointer), 0),
                new IsType(
                    new Register(1, TypeMetadata.Bool),
                    new Register(0, anyPointer),
                    TypeMetadata.I8
                ),
                new Return(new Register(1, TypeMetadata.Bool)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CastValueTypeTest()
    {
        const string code =
            """
            public test(obj: {}): i8 {
                return (i8)obj;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var any = typeProvider.GetType("{ }")!;
        var anyPointer = new TypePointerMetadata(any);

        var expected = new[]
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, anyPointer), 0),
                new Cast(
                    new Register(1, TypeMetadata.I8),
                    new Register(0, anyPointer),
                    TypeMetadata.I8
                ),
                new Return(new Register(1, TypeMetadata.I8)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void CastTest()
    {
        const string code =
            """
            public type Test { }

            public test(obj: {}): Test {
                return (Test)obj;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var any = typeProvider.GetType("{ }")!;
        var anyPointer = new TypePointerMetadata(any);

        var testType = (TypeMetadata)typeProvider.GetType("Test")!;
        var testPointer = new TypePointerMetadata(testType);

        var expected = new[]
        {
            new IrFunction("test", new Block("entry", [
                new LoadParameter(new Register(0, anyPointer), 0),
                new Cast(
                    new Register(1, testPointer),
                    new Register(0, anyPointer),
                    testType
                ),
                new Return(new Register(1, testPointer)),
            ])),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }

    [Test]
    public void ConditionalAndTest()
    {
        const string code =
            """
            public test(a: bool, b: bool): i32 {
                if (a && b) {
                    return 1;
                }

                return 0;
            }
            """;
        var (tree, typeProvider) = Parse(code);

        var ir = new IrGenerator();
        var functions = ir.Generate(typeProvider.Types, [tree]);

        var expected = new[]
        {
            new IrFunction("test", new Block(
                "entry",
                [
                    new LoadParameter(new Register(0, TypeMetadata.Bool), 0),
                    new LoadParameter(new Register(1, TypeMetadata.Bool), 1),
                    new Move(new Register(2, TypeMetadata.Bool), new Register(0, TypeMetadata.Bool)),
                    new Branch(new Register(2, TypeMetadata.Bool), "if_1_then", "if_1_end"),
                ],
                [
                    new Block("if_1_then", [
                        new Move(new Register(5, TypeMetadata.Bool), new Register(1, TypeMetadata.Bool)),
                        new Jump("if_1_end"),
                    ]),
                    new Block("if_1_end", [
                        new Phi(
                            new Register(6, TypeMetadata.Bool),
                            [
                                new Register(2, TypeMetadata.Bool),
                                new Register(5, TypeMetadata.Bool),
                            ]
                        ),
                        new Branch(new Register(6, TypeMetadata.Bool), "if_0_then", "if_0_end"),
                    ]),
                    new Block("if_0_then", [
                        new LoadConst(new Register(3, TypeMetadata.I32), 1),
                        new Return(new Register(3, TypeMetadata.I32)),
                        new Jump("if_0_end"),
                    ]),
                    new Block("if_0_end", [
                        new LoadConst(new Register(4, TypeMetadata.I32), 0),
                        new Return(new Register(4, TypeMetadata.I32)),
                    ]),
                ]
            )),
        };

        Assert.That(functions, Is.EqualTo(expected).Using(IrFunctionComparer.Instance));
    }
}