using Trilang.IntermediateRepresentation;
using Trilang.IntermediateRepresentation.Instructions;
using static Tri.Tests.Helpers;
using static Trilang.IntermediateRepresentation.Instructions.BinaryInstructionKind;

namespace Tri.Tests.IntermediateRepresentation;

public class IrGeneratorTests
{
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
              namespace Test1;

              public test(): i32 {
                  return 1 {{op}} 2;
              }
              """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        var expected =
            $"""
             function test_0_2af2b3192b419145:
             entry:
             	ldc	#0: i32, 1
             	ldc	#1: i32, 2
             	{@operator.ToString().ToLower()}	#2: i32, #0: i32, #1: i32
             	ret	#2: i32
             """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
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
              namespace Test1;

              public test(): bool {
                  return 1 {{op}} 2;
              }
              """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        var expected =
            $"""
             function test_0_cd2fd49bc6b014bd:
             entry:
             	ldc	#0: i32, 1
             	ldc	#1: i32, 2
             	{@operator.ToString().ToLower()}	#2: bool, #0: i32, #1: i32
             	ret	#2: bool
             """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void AssigmentOperatorTests()
    {
        const string code =
            """
            namespace Test1;

            public test(x: i32): void {
                x = 1;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: i32, 0
            	ldc	#1: i32, 1
            	mov	#2: i32, #1: i32
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
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
              namespace Test1;

              public test(x: i32): void {
                  x {{op}} 1;
              }
              """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        var expected =
            $"""
             function test_1_f22f9ef8659fbbff:
             entry:
             	ldp	#0: i32, 0
             	ldc	#1: i32, 1
             	{@operator.ToString().ToLower()}	#2: i32, #0: i32, #1: i32
             	mov	#3: i32, #2: i32
             	ret
             """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void AssignmentSsaFormTest()
    {
        const string code =
            """
            namespace Test1;

            public test(x: i32): i32 {
                x += 1;
                x = 10;

                return x;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	ldc	#1: i32, 1
            	add	#2: i32, #0: i32, #1: i32
            	mov	#4: i32, #2: i32
            	ldc	#3: i32, 10
            	mov	#5: i32, #3: i32
            	ret	#5: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void AddTwoParametersTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32, b: i32): i32 {
                return a + b;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_2_e24cc7bc82aab685:
            entry:
            	ldp	#0: i32, 0
            	ldp	#1: i32, 1
            	add	#2: i32, #0: i32, #1: i32
            	ret	#2: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void AddTwoVariablesTest()
    {
        const string code =
            """
            namespace Test1;

            public test(): i32 {
                var a: i32 = 1;
                var b: i32 = 2;

                return a + b;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_0_2af2b3192b419145:
            entry:
            	ldc	#0: i32, 1
            	mov	#1: i32, #0: i32
            	ldc	#2: i32, 2
            	mov	#3: i32, #2: i32
            	add	#4: i32, #1: i32, #3: i32
            	ret	#4: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void NullExpressionTest()
    {
        const string code =
            """
            namespace Test1;

            public test(): null {
                return null;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_0_5b9bc4ba528108e4:
            entry:
            	ldc	#0: null, null
            	ret	#0: null
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void UnaryMinusExpressionTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32): i32 {
                return -a;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	neg	#1: i32, #0: i32
            	ret	#1: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void LogicalNotExpressionTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: bool): bool {
                return !a;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_7772fb92073661c5:
            entry:
            	ldp	#0: bool, 0
            	not	#1: bool, #0: bool
            	ret	#1: bool
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void BitwiseNotExpressionTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32): i32 {
                return ~a;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	not	#1: i32, #0: i32
            	ret	#1: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ArrayAccessTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32[]): i32 {
                return a[0];
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_38a40cc70c225276:
            entry:
            	ldp	#0: i32[]*, 0
            	ldc	#1: i32, 0
            	get	#2: i32*, #0: i32[]*, #1: i32
            	ld	#3: i32, #2: i32*
            	ret	#3: i32
            function array_i32_<>_get_size_0_2ae1af192b331746:
            entry:
            	ldp	#0: i32[]*, 0
            	get	#1: i64*, #0: i32[]*, <>_size: i64
            	ld	#2: i64, #1: i64*
            	ret	#2: i64
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ArrayAccessWithExpressionTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32[], index: i32): i32 {
                return a[index + 2];
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_2_e9c73c3fbe34f858:
            entry:
            	ldp	#0: i32[]*, 0
            	ldp	#1: i32, 1
            	ldc	#2: i32, 2
            	add	#3: i32, #1: i32, #2: i32
            	get	#4: i32*, #0: i32[]*, #3: i32
            	ld	#5: i32, #4: i32*
            	ret	#5: i32
            function array_i32_<>_get_size_0_2ae1af192b331746:
            entry:
            	ldp	#0: i32[]*, 0
            	get	#1: i64*, #0: i32[]*, <>_size: i64
            	ld	#2: i64, #1: i64*
            	ret	#2: i64
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ArraySetElementTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: i32[]): void {
                a[0] = 10;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_cf03852a6cca2be2:
            entry:
            	ldp	#0: i32[]*, 0
            	ldc	#1: i32, 0
            	get	#2: i32*, #0: i32[]*, #1: i32
            	ldc	#3: i32, 10
            	st	#2: i32*, #3: i32
            	ret
            function array_i32_<>_get_size_0_2ae1af192b331746:
            entry:
            	ldp	#0: i32[]*, 0
            	get	#1: i64*, #0: i32[]*, <>_size: i64
            	ld	#2: i64, #1: i64*
            	ret	#2: i64
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CreateIntArrayTest()
    {
        const string code =
            """
            namespace Test1;

            public test(): i32[] {
                var a: i32[] = new i32[10];

                return a;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_0_e29ec11cb667c068:
            entry:
            	ldc	#0: i32, 10
            	alloc	#1: i32[]*, 8, 4, #0: i32
            	mov	#2: i32[]*, #1: i32[]*
            	ret	#2: i32[]*
            function array_i32_<>_get_size_0_2ae1af192b331746:
            entry:
            	ldp	#0: i32[]*, 0
            	get	#1: i64*, #0: i32[]*, <>_size: i64
            	ld	#2: i64, #1: i64*
            	ret	#2: i64
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CreateTypeTest()
    {
        const string code =
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) { }
            }

            public test(): Point {
                return new Point(1, 2);
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Point_ctor_d5a01a8bf898461f:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	ldp	#2: i32, 2
            	ret
            function test_0_8a439296ced9ed11:
            entry:
            	alloc	#0: Point*, 0
            	get	#1: (i32, i32) => void*, #0: Point*, ctor: (i32, i32) => void
            	ld	#2: (i32, i32) => void, #1: (i32, i32) => void*
            	ldc	#3: i32, 1
            	ldc	#4: i32, 2
            	call	#5: void, #2: (i32, i32) => void, (#3: i32, #4: i32)
            	ret	#0: Point*
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void IfStatementTest()
    {
        const string code =
            """
            namespace Test1;

            public max(a: i32, b: i32): i32 {
                if (a >= b) {
                    return a;
                } else {
                    return b;
                }
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function max_2_e24cc7bc82aab685:
            entry:
            	ldp	#0: i32, 0
            	ldp	#1: i32, 1
            	ge	#2: bool, #0: i32, #1: i32
            	br	#2: bool, if_0_then, if_0_else
            if_0_then: <- entry
            	ret	#0: i32
            	jmp	if_0_end
            if_0_else: <- entry
            	ret	#1: i32
            	jmp	if_0_end
            if_0_end: <- if_0_then, if_0_else

            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void IfStatementWithoutElseTest()
    {
        const string code =
            """
            namespace Test1;

            public max(a: i32): i32 {
                var b: i32 = 0;
                if (a > 0) {
                    b = 10;
                }

                return b;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function max_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	ldc	#1: i32, 0
            	mov	#2: i32, #1: i32
            	ldc	#3: i32, 0
            	gt	#4: bool, #0: i32, #3: i32
            	br	#4: bool, if_0_then, if_0_end
            if_0_then: <- entry
            	ldc	#5: i32, 10
            	mov	#6: i32, #5: i32
            	jmp	if_0_end
            if_0_end: <- entry, if_0_then
            	phi	#7: i32, #2: i32, #6: i32
            	ret	#7: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void NestedIfStatementsTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function max_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	ldc	#1: i32, 0
            	gt	#2: bool, #0: i32, #1: i32
            	br	#2: bool, if_0_then, if_0_else
            if_0_then: <- entry
            	ldc	#3: i32, 10
            	gt	#4: bool, #0: i32, #3: i32
            	br	#4: bool, if_1_then, if_1_else
            if_0_else: <- entry
            	ldc	#7: i32, 0
            	ret	#7: i32
            	jmp	if_0_end
            if_1_then: <- if_0_then
            	ldc	#5: i32, 1
            	ret	#5: i32
            	jmp	if_1_end
            if_1_else: <- if_0_then
            	ldc	#6: i32, 2
            	ret	#6: i32
            	jmp	if_1_end
            if_0_end: <- if_1_end, if_0_else

            if_1_end: <- if_1_then, if_1_else
            	jmp	if_0_end
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void IfStatementPhiFunctionTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_35acf129a21c9365:
            entry:
            	ldp	#0: i32, 0
            	ldc	#1: i32, 0
            	mov	#2: i32, #1: i32
            	ldc	#3: i32, 0
            	gt	#4: bool, #0: i32, #3: i32
            	br	#4: bool, if_0_then, if_0_else
            if_0_then: <- entry
            	ldc	#5: i32, 1
            	mov	#9: i32, #5: i32
            	jmp	if_0_end
            if_0_else: <- entry
            	ldc	#6: i32, 1
            	neg	#7: i32, #6: i32
            	mov	#8: i32, #7: i32
            	jmp	if_0_end
            if_0_end: <- if_0_then, if_0_else
            	phi	#10: i32, #8: i32, #9: i32
            	ret	#10: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void WhileStatementTest()
    {
        const string code =
            """
            namespace Test1;

            public test(): i32 {
                var i: i32 = 0;
                while (i < 10) {
                    i += 1;
                }

                return i;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_0_2af2b3192b419145:
            entry:
            	ldc	#0: i32, 0
            	mov	#1: i32, #0: i32
            	jmp	loop_0_start
            loop_0_start: <- entry, if_0_then
            	phi	#6: i32, #1: i32, #7: i32
            	ldc	#2: i32, 10
            	lt	#3: bool, #6: i32, #2: i32
            	br	#3: bool, if_0_then, loop_0_end
            if_0_then: <- loop_0_start
            	ldc	#4: i32, 1
            	add	#5: i32, #6: i32, #4: i32
            	mov	#7: i32, #5: i32
            	jmp	loop_0_start
            loop_0_end: <- loop_0_start
            	ret	#6: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallMethodTest()
    {
        const string code =
            """
            namespace Test1;

            public type Test {
                public method1(): i32 {
                    return 0;
                }

                public method2(): void {
                    this.method1();
                }
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Test_method1_0_2af2b3192b419145:
            entry:
            	ldp	#0: Test*, 0
            	ldc	#1: i32, 0
            	ret	#1: i32
            function Test_method2_0_3173c900e37ae1df:
            entry:
            	ldp	#0: Test*, 0
            	get	#1: () => i32*, #0: Test*, method1: () => i32
            	ld	#2: () => i32, #1: () => i32*
            	call	#3: i32, #2: () => i32
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallMethodWithVoidReturnTypeTest()
    {
        const string code =
            """
            namespace Test1;

            public type Test {
                public method1(): void { }

                public method2(): void {
                    this.method1();
                }
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Test_method1_0_3173c900e37ae1df:
            entry:
            	ldp	#0: Test*, 0
            	ret
            function Test_method2_0_3173c900e37ae1df:
            entry:
            	ldp	#0: Test*, 0
            	get	#1: () => void*, #0: Test*, method1: () => void
            	ld	#2: () => void, #1: () => void*
            	call	#3: void, #2: () => void
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallSetterTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Point_<>_get_x_0_2af2b3192b419145:
            entry:
            	ldp	#0: Point*, 0
            	get	#1: i32*, #0: Point*, <>_x: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Point_<>_set_x_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Point*, <>_x: i32
            	st	#2: i32*, #1: i32
            function Point_ctor_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: (i32) => void*, #0: Point*, <>_set_x: (i32) => void
            	ld	#3: (i32) => void, #2: (i32) => void*
            	call	#4: void, #3: (i32) => void, (#1: i32)
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallGetterTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Point_ctor_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: (i32) => void*, #0: Point*, <>_set_x: (i32) => void
            	ld	#3: (i32) => void, #2: (i32) => void*
            	call	#4: void, #3: (i32) => void, (#1: i32)
            	ret
            function test_0_2af2b3192b419145:
            entry:
            	alloc	#0: Point*, 4
            	get	#1: (i32) => void*, #0: Point*, ctor: (i32) => void
            	ld	#2: (i32) => void, #1: (i32) => void*
            	ldc	#3: i32, 1
            	call	#4: void, #2: (i32) => void, (#3: i32)
            	mov	#5: Point*, #0: Point*
            	get	#6: () => i32*, #5: Point*, <>_get_x: () => i32
            	ld	#7: () => i32, #6: () => i32*
            	call	#8: i32, #7: () => i32
            	ret	#8: i32
            function Point_<>_get_x_0_2af2b3192b419145:
            entry:
            	ldp	#0: Point*, 0
            	get	#1: i32*, #0: Point*, <>_x: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Point_<>_set_x_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Point*, <>_x: i32
            	st	#2: i32*, #1: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallUnaryOpTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Point_ctor_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: (i32) => void*, #0: Point*, <>_set_x: (i32) => void
            	ld	#3: (i32) => void, #2: (i32) => void*
            	call	#4: void, #3: (i32) => void, (#1: i32)
            	ret
            function test_0_2af2b3192b419145:
            entry:
            	alloc	#0: Point*, 4
            	get	#1: (i32) => void*, #0: Point*, ctor: (i32) => void
            	ld	#2: (i32) => void, #1: (i32) => void*
            	ldc	#3: i32, 1
            	call	#4: void, #2: (i32) => void, (#3: i32)
            	mov	#5: Point*, #0: Point*
            	get	#6: () => i32*, #5: Point*, <>_get_x: () => i32
            	ld	#7: () => i32, #6: () => i32*
            	call	#8: i32, #7: () => i32
            	neg	#9: i32, #8: i32
            	ret	#9: i32
            function Point_<>_get_x_0_2af2b3192b419145:
            entry:
            	ldp	#0: Point*, 0
            	get	#1: i32*, #0: Point*, <>_x: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Point_<>_set_x_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Point*, <>_x: i32
            	st	#2: i32*, #1: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallBinaryOpTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Point_ctor_d5a01a8bf898461f:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	ldp	#2: i32, 2
            	get	#3: (i32) => void*, #0: Point*, <>_set_x: (i32) => void
            	ld	#4: (i32) => void, #3: (i32) => void*
            	call	#5: void, #4: (i32) => void, (#1: i32)
            	get	#6: (i32) => void*, #0: Point*, <>_set_y: (i32) => void
            	ld	#7: (i32) => void, #6: (i32) => void*
            	call	#8: void, #7: (i32) => void, (#2: i32)
            	ret
            function test_0_2af2b3192b419145:
            entry:
            	alloc	#0: Point*, 8
            	get	#1: (i32, i32) => void*, #0: Point*, ctor: (i32, i32) => void
            	ld	#2: (i32, i32) => void, #1: (i32, i32) => void*
            	ldc	#3: i32, 1
            	ldc	#4: i32, 2
            	call	#5: void, #2: (i32, i32) => void, (#3: i32, #4: i32)
            	mov	#6: Point*, #0: Point*
            	get	#7: () => i32*, #6: Point*, <>_get_x: () => i32
            	ld	#8: () => i32, #7: () => i32*
            	call	#9: i32, #8: () => i32
            	get	#10: () => i32*, #6: Point*, <>_get_y: () => i32
            	ld	#11: () => i32, #10: () => i32*
            	call	#12: i32, #11: () => i32
            	add	#13: i32, #9: i32, #12: i32
            	ret	#13: i32
            function Point_<>_get_x_0_2af2b3192b419145:
            entry:
            	ldp	#0: Point*, 0
            	get	#1: i32*, #0: Point*, <>_x: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Point_<>_set_x_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Point*, <>_x: i32
            	st	#2: i32*, #1: i32
            function Point_<>_get_y_0_2af2b3192b419145:
            entry:
            	ldp	#0: Point*, 0
            	get	#1: i32*, #0: Point*, <>_y: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Point_<>_set_y_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Point*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Point*, <>_y: i32
            	st	#2: i32*, #1: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallComplexTypeAccessTest()
    {
        const string code =
            """
            namespace Test1;

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
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Test1_ctor_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Test1*, 0
            	ldp	#1: i32, 1
            	get	#2: (i32) => void*, #0: Test1*, <>_set_x: (i32) => void
            	ld	#3: (i32) => void, #2: (i32) => void*
            	call	#4: void, #3: (i32) => void, (#1: i32)
            	ret
            function Test2_ctor_d0266fe934ea148e:
            entry:
            	ldp	#0: Test2*, 0
            	ldp	#1: Test1*, 1
            	get	#2: (Test1) => void*, #0: Test2*, <>_set_obj: (Test1) => void
            	ld	#3: (Test1) => void, #2: (Test1) => void*
            	call	#4: void, #3: (Test1) => void, (#1: Test1*)
            	ret
            function test_0_2af2b3192b419145:
            entry:
            	alloc	#0: Test1*, 4
            	get	#1: (i32) => void*, #0: Test1*, ctor: (i32) => void
            	ld	#2: (i32) => void, #1: (i32) => void*
            	ldc	#3: i32, 1
            	call	#4: void, #2: (i32) => void, (#3: i32)
            	mov	#5: Test1*, #0: Test1*
            	alloc	#6: Test2*, 8
            	get	#7: (Test1) => void*, #6: Test2*, ctor: (Test1) => void
            	ld	#8: (Test1) => void, #7: (Test1) => void*
            	call	#9: void, #8: (Test1) => void, (#5: Test1*)
            	mov	#10: Test2*, #6: Test2*
            	get	#11: () => Test1*, #10: Test2*, <>_get_obj: () => Test1
            	ld	#12: () => Test1, #11: () => Test1*
            	call	#13: Test1*, #12: () => Test1
            	get	#14: () => i32*, #13: Test1*, <>_get_x: () => i32
            	ld	#15: () => i32, #14: () => i32*
            	call	#16: i32, #15: () => i32
            	ret	#16: i32
            function Test1_<>_get_x_0_2af2b3192b419145:
            entry:
            	ldp	#0: Test1*, 0
            	get	#1: i32*, #0: Test1*, <>_x: i32
            	ld	#2: i32, #1: i32*
            	ret	#2: i32
            function Test1_<>_set_x_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: Test1*, 0
            	ldp	#1: i32, 1
            	get	#2: i32*, #0: Test1*, <>_x: i32
            	st	#2: i32*, #1: i32
            function Test2_<>_get_obj_0_df4563aec012855c:
            entry:
            	ldp	#0: Test2*, 0
            	get	#1: Test1**, #0: Test2*, <>_obj: Test1
            	ld	#2: Test1*, #1: Test1**
            	ret	#2: Test1*
            function Test2_<>_set_obj_1_d0266fe934ea148e:
            entry:
            	ldp	#0: Test2*, 0
            	ldp	#1: Test1*, 1
            	get	#2: Test1**, #0: Test2*, <>_obj: Test1
            	st	#2: Test1**, #1: Test1*
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallFunctionTest()
    {
        const string code =
            """
            namespace Test1;

            public main(): void {
                test1();
            }

            public test1(): void { }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function main_0_3173c900e37ae1df:
            entry:
            	get	#0: () => void*, test1: () => void
            	ld	#1: () => void, #0: () => void*
            	call	#2: void, #1: () => void
            	ret
            function test1_0_3173c900e37ae1df:
            entry:
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallStaticMethodTest()
    {
        const string code =
            """
            namespace Test1;

            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function Test_test_s_0_3173c900e37ae1df:
            entry:
            	ret
            function main_0_3173c900e37ae1df:
            entry:
            	get	#0: () => void*, test: () => void
            	ld	#1: () => void, #0: () => void*
            	call	#2: void, #1: () => void
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallViaVariableTest()
    {
        const string code =
            """
            namespace Test1;

            public test(): void {}

            public main(): void {
                var f: () => void = test;
                f();
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_0_3173c900e37ae1df:
            entry:
            	ret
            function main_0_3173c900e37ae1df:
            entry:
            	get	#0: () => void*, test: () => void
            	ld	#1: () => void, #0: () => void*
            	mov	#2: () => void, #1: () => void
            	call	#3: void, #2: () => void
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallCallbackTest()
    {
        const string code =
            """
            namespace Test1;

            public test(callback: () => void): void {
                callback();
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_2662accf658c5c8b:
            entry:
            	ldp	#0: () => void, 0
            	call	#1: void, #0: () => void
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CallInterfaceMethodTest()
    {
        const string code =
            """
            namespace Test1;

            public type Interface = {
                method(): void;
            }

            public test(p: Interface): void {
                p.method();
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_cf064f170ff86101:
            entry:
            	ldp	#0: Interface*, 0
            	get	#1: () => void*, #0: Interface*, method: () => void
            	ld	#2: () => void, #1: () => void*
            	call	#3: void, #2: () => void
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void IsTypeTest()
    {
        const string code =
            """
            namespace Test1;

            public test(obj: {}): bool {
                return obj is i8;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_31fc2f6c0e066efc:
            entry:
            	ldp	#0: { }*, 0
            	is	#1: bool, #0: { }*, i8
            	ret	#1: bool
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CastValueTypeTest()
    {
        const string code =
            """
            namespace Test1;

            public test(obj: {}): i8 {
                return (i8)obj;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_2baa3d192bdd861d:
            entry:
            	ldp	#0: { }*, 0
            	cast	#1: i8, #0: { }*, i8
            	ret	#1: i8
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void CastTest()
    {
        const string code =
            """
            namespace Test1;

            public type Test { }

            public test(obj: {}): Test {
                return (Test)obj;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_695e7ecb50fc0a40:
            entry:
            	ldp	#0: { }*, 0
            	cast	#1: Test*, #0: { }*, Test
            	ret	#1: Test*
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ConditionalAndTest()
    {
        const string code =
            """
            namespace Test1;

            public test(a: bool, b: bool): i32 {
                if (a && b) {
                    return 1;
                }

                return 0;
            }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_2_2471114b9902d8e5:
            entry:
            	ldp	#0: bool, 0
            	ldp	#1: bool, 1
            	mov	#2: bool, #0: bool
            	br	#2: bool, if_1_then, if_1_end
            if_1_then: <- entry
            	mov	#5: bool, #1: bool
            	jmp	if_1_end
            if_1_end: <- entry, if_1_then
            	phi	#6: bool, #2: bool, #5: bool
            	br	#6: bool, if_0_then, if_0_end
            if_0_then: <- if_1_end
            	ldc	#3: i32, 1
            	ret	#3: i32
            	jmp	if_0_end
            if_0_end: <- if_1_end, if_0_then
            	ldc	#4: i32, 0
            	ret	#4: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void FunctionOverloadTest()
    {
        const string code =
            """
            namespace Test1;

            public test(x: i32): void { }
            public test(x: bool): void { }
            """;
        var (tree, diagnostics, compilationContext) = Lower(CreateFile(code));

        var ir = new IrGenerator(new HashSet<string>(), compilationContext);
        var functions = ir.Generate([tree]);

        const string expected =
            """
            function test_1_f22f9ef8659fbbff:
            entry:
            	ldp	#0: i32, 0
            	ret
            function test_1_c79f1eebc0a6b477:
            entry:
            	ldp	#0: bool, 0
            	ret
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions.Dump(), Is.EqualTo(expected).NoClip);
    }
}