using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
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
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "main",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata());

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionParameterTypesTest()
    {
        var (tree, diagnostics) = Parse("public main(a: i32, b: bool): void { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "main",
            [
                new ParameterMetadata(null, "a", TypeMetadata.I32),
                new ParameterMetadata(null, "b", TypeMetadata.Bool)
            ],
            new FunctionTypeMetadata(null, [TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void),
            new FunctionGroupMetadata());

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(
            function.Parameters[0].Type.Metadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
        Assert.That(
            function.Parameters[1].Type.Metadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var variable = semanticTree.Find<VariableDeclaration>();
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.Type.Metadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForIncorrectVariableTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: xxx = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void SetMetadataForTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32;
                y: i32;

                public toString(): void {
                }

                public distance(other: i32): i32 {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new TypeMetadata(null, "Point");
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expected)));

        var xProperty = new PropertyMetadata(
            null,
            expected,
            "x",
            TypeMetadata.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = new PropertyMetadata(
            null,
            expected,
            "y",
            TypeMetadata.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "distance",
            [new ParameterMetadata(null, "other", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32),
            new FunctionGroupMetadata()));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var (tree, diagnostics) = Parse("public type MyInt = i32;");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new TypeAliasMetadata(null, "MyInt", [], TypeMetadata.I32);
        var semanticTree = semanticTrees.Single();
        var node = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse("public type MyF = (i32, bool) => f64;");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionTypeMetadata(
            null,
            [TypeMetadata.I32, TypeMetadata.Bool],
            TypeMetadata.F64);
        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<FunctionType>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public add(a: i32, b: i32): i32 {
                return 0;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "add",
            [
                new ParameterMetadata(null, "a", TypeMetadata.I32),
                new ParameterMetadata(null, "b", TypeMetadata.I32)
            ],
            new FunctionTypeMetadata(null, [TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32),
            new FunctionGroupMetadata());

        var semanticTree = semanticTrees.Single();
        var node = semanticTree.Find<FunctionDeclaration>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForForwardDefinedFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test1(): void {
                test2();
            }

            public test2(): void { }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test2",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata());

        var semanticTree = semanticTrees.Single();
        var node = semanticTree.Find<FunctionDeclaration>(x => x.Name == "test2");
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));

        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralNumberTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32 {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralFloatTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): f64 {
                return 3.14;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.F64).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): bool {
                return true;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralCharTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): char {
                return 'x';
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Char).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralStringTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): string {
                return "xxx";
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.String).Using(new MetadataComparer()));
    }

    [Test]
    public void ReturnStatementTypeDoesntMatchTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): bool {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0004ReturnTypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(35, 2, 14))),
            "Return type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void UnaryPlusTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32 {
                return -1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32 {
                return -1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): bool {
                return !true;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32 {
                return 1 + 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var binaryNode = semanticTree.Find<BinaryExpression>();
        Assert.That(binaryNode, Is.Not.Null);
        Assert.That(
            binaryNode.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotIncorrectOperandTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32 {
                return !1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0010IncompatibleUnaryOperator,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(32, 2, 12), new SourcePosition(34, 2, 14))),
            "Incompatible operand type 'i32' for operator 'LogicalNot'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(a: i32): i32 {
                return a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = true;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(43, 2, 22))),
            "Type mismatch: expected 'i32', got 'bool'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                if (1) {
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(30, 2, 9), new SourcePosition(31, 2, 10))),
            "Type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionCallIncorrectParameterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public add(a: i32): i32 {
                return 0;
            }

            public main(): i32 {
                return add(true);
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(79, 6, 16), new SourcePosition(83, 6, 20))),
            "Type mismatch: expected 'i32', got 'bool'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                while (1) {
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13))),
            "Type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ReturnInConstructorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor() {
                    return;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void ReturnWithExpressionInConstructorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor() {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0004ReturnTypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(55, 3, 9), new SourcePosition(64, 3, 18))),
            "Return type mismatch: expected 'void', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void SetMetadataForInterfaceTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var interfaceType = new InterfaceMetadata(null);
        var expected = new TypeAliasMetadata(null, "Point", [], interfaceType);

        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                null,
                interfaceType,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                null));
        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                null,
                interfaceType,
                "y",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                null));
        interfaceType.AddMethod(
            new InterfaceMethodMetadata(
                null,
                interfaceType,
                "distance",
                new FunctionTypeMetadata(null, [expected], TypeMetadata.F64),
                new FunctionGroupMetadata()));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetReturnTypeForVariableWithFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public add(a: i32, b: i32): i32 {
                return 1;
            }

            public main(): void {
                var x: (i32, i32) => i32 = add;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionTypeMetadata(
            null,
            [TypeMetadata.I32, TypeMetadata.I32],
            TypeMetadata.I32);
        var semanticTree = semanticTrees.Single();
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void CallNonFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): void {
                a();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0007ExpectedFunction,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(33, 2, 6))),
            "Expected a function, got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ThisReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): void {
                    this;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "this");
        var pointType = typeProvider.GetType("Point");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithMultipleMembersReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                a: i32;

                public toString(): void {
                    this.a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithIncorrectPropertyNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                a: i32;

                public toString(): void {
                    this.x;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(71, 5, 9), new SourcePosition(77, 5, 15))),
            "The 'Point' type doesn't have 'x'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void InterfaceMemberAccessReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.x;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var pointType = typeProvider.GetType("Point");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "x");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void InterfaceMemberAccessIncorrectPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.c;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(77, 6, 12), new SourcePosition(80, 6, 15))),
            "The 'Point' type doesn't have 'c'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AliasFunctionTypeMemberAccessReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type F = () => void;

            public type Test {
                f: F;
            }

            public test(a: Test): F {
                return a.f;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var pointType = typeProvider.GetType("Test");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "f");
        var functionType = typeProvider.GetType("F");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(functionType).Using(new MetadataComparer()));
    }

    [Test]
    public void NewOperatorSetCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point = new Point(1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Point") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var ctor = type.GetConstructor([TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(ctor, Is.Not.Null);

        var semanticTree = semanticTrees.Single();
        var newOp = semanticTree.Find<NewObjectExpression>();
        Assert.That(newOp, Is.Not.Null);
        Assert.That(newOp.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void NewOperatorForInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
            }

            public test(): void {
                var a: Point = new Point(1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0009CantCreateObject,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(90, 7, 20), new SourcePosition(105, 7, 35))),
            "Cannot create an instance of type 'Point'");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NewOperatorMissingConstructorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point = new Point(2);
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(111, 7, 20), new SourcePosition(123, 7, 32))),
            "The 'Point' type doesn't have 'i32' constructor.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void SetMetadataForDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type DU = {} | i32 | () => void;
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var du = new DiscriminatedUnionMetadata(null, [
            new InterfaceMetadata(null),
            TypeMetadata.I32,
            new FunctionTypeMetadata(null, [], TypeMetadata.Void)
        ]);
        var alias = new TypeAliasMetadata(null, "DU", [], du);

        var semanticTree = semanticTrees.Single();
        var aliasNode = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(aliasNode.Metadata, Is.EqualTo(alias).Using(new MetadataComparer()));

        var duNode = semanticTree.Find<DiscriminatedUnion>();
        Assert.That(duNode, Is.Not.Null);
        Assert.That(duNode.Metadata, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void AccessArrayElementTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32[]): i32 {
                return a[1];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var arrayAccess = semanticTree.Find<ArrayAccessExpression>();
        Assert.That(arrayAccess, Is.Not.Null);
        Assert.That(arrayAccess.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UseArrayAccessorOnNotArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): void {
                return a[1];
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0006ExpectedArray,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13))),
            "Expected an array, got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void UseArrayAccessorWithNonNumberTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32[]): i32 {
                return a["xxx"];
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(42, 2, 14), new SourcePosition(47, 2, 19))),
            "Type mismatch: expected 'i32', got 'string'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForExpressionTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): (i32, i32) {
                return (1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new TupleMetadata(null, [TypeMetadata.I32, TypeMetadata.I32]);

        var actual = typeProvider.GetType("(i32, i32)");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void NewArrayReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): i32[] {
                return new i32[10];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new ArrayMetadata(null, TypeMetadata.I32);
        var semanticTree = semanticTrees.Single();
        var newArray = semanticTree.Find<NewArrayExpression>();
        Assert.That(newArray, Is.Not.Null);
        Assert.That(newArray.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                x: T;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);
        Assert.That(
            property.Metadata.Type,
            Is.EqualTo(new TypeArgumentMetadata(null, "T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericArrayPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                x: T[];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);

        var typeArrayMetadata = new ArrayMetadata(null, new TypeArgumentMetadata(null, "T"));
        Assert.That(property.Metadata.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForClosedGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {}
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var closedType = typeProvider.GetType("List<i32>");
        var semanticTree = semanticTrees.Single();
        var genericTypeNode = semanticTree.Find<GenericType>();
        Assert.That(closedType, Is.Not.Null);
        Assert.That(genericTypeNode, Is.Not.Null);
        Assert.That(genericTypeNode.Metadata, Is.EqualTo(closedType).Using(new MetadataComparer()));
    }

    [Test]
    public void FindCtorInGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                a: T;
            }

            public main(): void {
                var x: Test<i32> = new Test<i32>();
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var closedType = typeProvider.GetType("Test<i32>") as TypeMetadata;
        var ctor = closedType!.GetConstructor([]);

        var semanticTree = semanticTrees.Single();
        var newObj = semanticTree.Find<NewObjectExpression>();
        Assert.That(newObj, Is.Not.Null);
        Assert.That(newObj.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void SetMetadataForClosedGenericTypeFieldTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                a: T;
            }

            public main(): i32 {
                var x: Test<i32> = new Test<i32>();
                return x.a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnStmt = semanticTree.Find<ReturnStatement>();
        Assert.That(returnStmt, Is.Not.Null);
        Assert.That(returnStmt.Expression, Is.Not.Null);
        Assert.That(
            returnStmt.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void MemberAccessNestedCallTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test1 {
                public b(): Test2 {
                    return new Test2();
                }
            }

            public type Test2 {
                c: i32;
            }

            public test(a: Test1): i32 {
                return a.b().c;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void SetMetadataForStaticClassTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public static test(): void {
                }
            }

            public main(): void {
                Test.test();
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test");
        Assert.That(type, Is.Not.Null);

        var functionType = typeProvider.GetType("() => void");
        Assert.That(functionType, Is.Not.Null);

        var semanticTree = semanticTrees.Single();
        var staticTypeMember = semanticTree.Find<MemberAccessExpression>(x => x.Name == "Test");
        Assert.That(staticTypeMember, Is.Not.Null);
        Assert.That(staticTypeMember.ReturnTypeMetadata, Is.EqualTo(type));

        var member = semanticTree.Find<MemberAccessExpression>(x => x.Name == "test");
        Assert.That(member, Is.Not.Null);
        Assert.That(member.ReturnTypeMetadata, Is.EqualTo(functionType));
    }

    [Test]
    public void TupleMemberAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: (i32, string)): string {
                return a.1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var tupleMember = semanticTree.Find<MemberAccessExpression>();
        Assert.That(tupleMember, Is.Not.Null);
        Assert.That(tupleMember.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.String).Using(new MetadataComparer()));
    }

    [Test]
    public void TupleMemberAccessIndexOutsideTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: (i32, string)): string {
                return a.2;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(51, 2, 12), new SourcePosition(54, 2, 15))),
            "The '(i32, string)' type doesn't have '2'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CastExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): i8 {
                return (i8)a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var castExp = semanticTree.Find<CastExpression>();
        Assert.That(castExp, Is.Not.Null);
        Assert.That(castExp.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I8).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method1(): void {
                    method2();
                }

                public method2(): void {}
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = (TypeMetadata)typeProvider.GetType("Test")!;
        var method = type.GetMethod("method2")!.Functions[0];

        var semanticTree = semanticTrees.Single();
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationInCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public constructor() {
                    method2();
                }

                public method2(): void {}
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = (TypeMetadata)typeProvider.GetType("Test")!;
        var method = type.GetMethod("method2")!.Functions[0];

        var semanticTree = semanticTrees.Single();
        var memberAccess = semanticTree.Find<MemberAccessExpression>()!;
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UsePropertyBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method1(): i32 {
                    return prop;
                }

                prop: i32;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test")!;
        var method = type.GetMember("prop")!;

        var semanticTree = semanticTrees.Single();
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void CallFunctionOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(x: i32): void { }

            public test(x: bool): void { }

            public main(): void {
                test(1);
                test(true);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var functionGroup = new FunctionGroupMetadata();
        var function1 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var function2 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", TypeMetadata.Bool)],
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(function1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(function2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(function1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(function2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallMethodOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method(x: i32): void { }

                public method(x: bool): void { }

                public test(): void {
                    method(1);
                    method(true);
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new TypeMetadata(null, "Test");
        var functionGroup = new FunctionGroupMetadata();
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", TypeMetadata.Bool)],
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallMethodOverloadOutsideTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method(x: i32): void { }

                public method(x: bool): void { }
            }

            public test(o: Test): void {
                o.method(1);
                o.method(true);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new TypeMetadata(null, "Test");
        var functionGroup = new FunctionGroupMetadata();
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", TypeMetadata.Bool)],
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallInterfaceMethodOverloadOutsideTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                method(i32): void;
                method(bool): void;
            }

            public test(o: Test): void {
                o.method(1);
                o.method(true);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new InterfaceMetadata(null);
        var functionGroup = new FunctionGroupMetadata();
        var method1 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var method2 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void PassFunctionGroupAsParameterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public f(x: i32): void { }
            public f(x: bool): void { }
            public test(func: (bool) => void): void { }

            public main(): void {
                test(f);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var call = semanticTree.Find<CallExpression>()!;
        var parameter = (MemberAccessExpression)call.Parameters[0];

        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "f",
            [new ParameterMetadata(null, "x", TypeMetadata.Bool)],
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            new FunctionGroupMetadata());

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(parameter.Reference, Is.EqualTo(function).Using(new MetadataComparer()));
    }

    [Test]
    public void PassFunctionGroupAsParameterFailTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public f(x: i32): void { }
            public f(x: f64): void { }
            public test(func: (bool) => void): void { }

            public main(): void {
                test(f);
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0023NoSuitableOverload,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(130, 6, 10), new SourcePosition(131, 6, 11))),
            "No suitable overload found for 'f'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}