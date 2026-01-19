using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests.Semantics;

public class MetadataGeneratorTests
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
    public void GenerateMetadataForTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32;
                y: i32;

                public constructor(x: i32, y: i32) {}

                public toString(): string {
                    return "hello";
                }

                private distance(other: i32): i32 {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new TypeMetadata(null, "Point");

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

        expected.AddConstructor(new ConstructorMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            [
                new ParameterMetadata(null, "x", TypeMetadata.I32),
                new ParameterMetadata(null, "y", TypeMetadata.I32)
            ],
            new FunctionTypeMetadata(null, [TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.Void)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.String),
            new FunctionGroupMetadata()));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Private,
            false,
            "distance",
            [new ParameterMetadata(null, "other", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32),
            new FunctionGroupMetadata()));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));

        var toStringType = typeProvider.GetType("() => string");
        var expectedToStringType = new FunctionTypeMetadata(null, [], TypeMetadata.String);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType).Using(new MetadataComparer()));

        var distanceType = typeProvider.GetType("(i32) => i32");
        var expectedDistanceType = new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                x: i32 { public get; public set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var typeMetadata = new TypeMetadata(null, "Test");
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], typeMetadata)));
        var propertyMetadata = new PropertyMetadata(
            null,
            typeMetadata,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);
        typeMetadata.AddMethod(propertyMetadata.Getter!);
        typeMetadata.AddMethod(propertyMetadata.Setter!);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actual = typeProvider.GetType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata).Using(new MetadataComparer()));

        var actualProperty = actual.GetProperty("x");
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = { }
            public type Interface2 = { }
            public type Point : Interface1, Interface2 { }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var interfaceMetadata = new InterfaceMetadata(null);
        var expected = new TypeMetadata(
            null,
            "Point",
            [],
            [interfaceMetadata],
            [],
            [],
            [],
            []);
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expected)));
        var actual = typeProvider.GetType("Point");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingPropertyTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: xxx;
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
                new SourceSpan(new SourcePosition(27, 2, 8), new SourcePosition(30, 2, 11))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public distance(other: xxx): f64 {
                    return;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0003UnknownType,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(47, 2, 28), new SourcePosition(50, 2, 31))),
                "Unknown type: 'xxx'."),
            new Diagnostic(
                DiagnosticId.S0004ReturnTypeMismatch,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(67, 3, 9), new SourcePosition(74, 3, 16))),
                "Return type mismatch: expected 'f64', got 'void'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): xxx {
                    return;
                }
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
                new SourceSpan(new SourcePosition(43, 2, 24), new SourcePosition(46, 2, 27))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type MyInt = i32;");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new AliasMetadata(null, "MyInt", [], TypeMetadata.I32);
        var actual = typeProvider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("public type MyInt = xxx;");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(23, 1, 24))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(arr: i32[]): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new ArrayMetadata(null, TypeMetadata.I32);
        var actual = typeProvider.GetType("i32[]");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(arr: xxx): void {
                return;
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
                new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32, b: i32): i32 {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new FunctionTypeMetadata(null, [TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        var actual = typeProvider.GetType("(i32, i32) => i32");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: xxx): void {
                return;
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
                new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): xxx {
                return;
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
                new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {}
            public type MyPoint = Point;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedType = new TypeMetadata(null, "Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expectedType)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType);
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type MyPoint = Point;
            public type Point {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedType = new TypeMetadata(null, "Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expectedType)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType);
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {}
            public type MyPoint = Point[];
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedType = new TypeMetadata(null, "Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expectedType)));

        var expectedArrayType = new ArrayMetadata(null, expectedType);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType);
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type MyPoint = Point[];
            public type Point {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedType = new TypeMetadata(null, "Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expectedType)));

        var expectedArrayType = new ArrayMetadata(null, expectedType);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType);
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForInterfaceType()
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
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedInterface = new InterfaceMetadata(null);
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                null));
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "y",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                null));

        var expectedAlias = new AliasMetadata(null, "Point", [], expectedInterface);
        expectedInterface.AddMethod(new InterfaceMethodMetadata(
            null,
            expectedInterface,
            "distance",
            new FunctionTypeMetadata(null, [expectedAlias], TypeMetadata.F64),
            new FunctionGroupMetadata()));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualInterface = typeProvider.GetType(expectedInterface.ToString());
        Assert.That(actualInterface, Is.EqualTo(expectedInterface).Using(new MetadataComparer()));

        var actualAlias = typeProvider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type DU = {} | i32 | () => void;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var du = new DiscriminatedUnionMetadata(null, [
            new InterfaceMetadata(null),
            TypeMetadata.I32,
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
        ]);
        var alias = new AliasMetadata(null, "DU", [], du);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualAlias = typeProvider.GetType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualDu = typeProvider.GetType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("public type Tuple = (i32, f64);");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var tuple = new TupleMetadata(null, [TypeMetadata.I32, TypeMetadata.F64]);
        var alias = new AliasMetadata(null, "Tuple", [], tuple);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("public type Tuple = (i32, (f64, bool));");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var nestedTuple = new TupleMetadata(null, [TypeMetadata.F64, TypeMetadata.Bool]);
        var tuple = new TupleMetadata(null, [TypeMetadata.I32, nestedTuple]);
        var alias = new AliasMetadata(null, "Tuple", [], tuple);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(i32, (f64, bool))");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualNestedTuple = typeProvider.GetType("(f64, bool)");
        Assert.That(actualNestedTuple, Is.EqualTo(nestedTuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuInTupleTest()
    {
        var (tree, diagnostics) = Parse("public type Tuple = (i32, bool | i8);");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var du = new DiscriminatedUnionMetadata(null, [TypeMetadata.Bool, TypeMetadata.I8]);
        var tuple = new TupleMetadata(null, [TypeMetadata.I32, du]);
        var alias = new AliasMetadata(null, "Tuple", [], tuple);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(i32, bool | i8)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = typeProvider.GetType("bool | i8");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleInDuTest()
    {
        var (tree, diagnostics) = Parse("public type Tuple = i32 | (f64, bool);");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var tuple = new TupleMetadata(null, [TypeMetadata.F64, TypeMetadata.Bool]);
        var du = new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, tuple]);
        var alias = new AliasMetadata(null, "Tuple", [], du);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(f64, bool)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = typeProvider.GetType("i32 | (f64, bool)");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericTypeTest()
    {
        var (tree, diagnostics) = Parse("public type Test<T1, T2> {}");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expected = new TypeMetadata(null, "Test");
        expected.AddGenericArgument(new TypeArgumentMetadata(null, "T1"));
        expected.AddGenericArgument(new TypeArgumentMetadata(null, "T2"));
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expected)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actual = typeProvider.GetType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                x: T;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = type.GetProperty("x");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new TypeArgumentMetadata(null, "T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyInWrongTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test1<T> {}
            public type Test2 {
                x: T;
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
                new SourceSpan(new SourcePosition(51, 3, 8), new SourcePosition(52, 3, 9))),
            "Unknown type: 'T'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForGenericArrayPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> {
                x: T[];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = type.GetProperty("x");
        Assert.That(property, Is.Not.Null);

        var typeArrayMetadata = new ArrayMetadata(null, new TypeArgumentMetadata(null, "T"));
        Assert.That(property.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {}
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var closedType = typeProvider.GetType("List<i32>");

        var expected = new TypeMetadata(null, "List");
        expected.AddGenericArgument(TypeMetadata.I32);
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], expected)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(closedType, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                Prop: T;
            }
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var closedType = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = closedType!.GetProperty("Prop");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateDefaultCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("Test") as TypeMetadata;
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Constructors, Has.Count.EqualTo(1));

        var ctor = type.GetConstructor([]);
        Assert.That(ctor, Is.Not.Null);
        Assert.That(ctor.AccessModifier, Is.EqualTo(AccessModifierMetadata.Public));
    }

    [Test]
    public void GenerateInlineClosedGenericArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                prop: T[];
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new ArrayMetadata(null, TypeMetadata.I32)).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                prop: (T, i32);
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(new TupleMetadata(null, [TypeMetadata.I32, TypeMetadata.I32])).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                prop: T | i32;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        var expected = new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                prop: () => T;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(new FunctionTypeMetadata(null, [], TypeMetadata.I32)).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {
                prop: { x: T; };
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        var expected = new InterfaceMetadata(null);
        expected.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expected,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                null));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateHighOrderFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: ((i32) => void) => void): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("((i32) => void) => void");
        var expected = new FunctionTypeMetadata(
            null,
            [new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void)],
            TypeMetadata.Void);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericAliasToDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse("public type Test<T> = i32 | T;");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<>");
        var du = new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, new TypeArgumentMetadata(null, "T")]);
        var expected = new AliasMetadata(null, "Test", [new TypeArgumentMetadata(null, "T")], du);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> = i32 | T;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");
        var du = new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new AliasMetadata(null, "Test", [TypeMetadata.I32], du);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> = () => T;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");
        var functionType = new FunctionTypeMetadata(null, [], TypeMetadata.I32);
        var expected = new AliasMetadata(null, "Test", [TypeMetadata.I32], functionType);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> = { x: T; }

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");
        var interfaceType = new InterfaceMetadata(null);
        interfaceType.AddProperty(new InterfacePropertyMetadata(
            null,
            interfaceType,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            null));
        var expected = new AliasMetadata(null, "Test", [TypeMetadata.I32], interfaceType);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> = (i32, T);

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");
        var tuple = new TupleMetadata(null, [TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new AliasMetadata(null, "Test", [TypeMetadata.I32], tuple);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test<T> = T[];

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");
        var array = new ArrayMetadata(null, TypeMetadata.I32);
        var expected = new AliasMetadata(null, "Test", [TypeMetadata.I32], array);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");

        var listMetadata = new TypeMetadata(null, "List", [TypeMetadata.I32], [], [], [], [], []);
        listMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                listMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], listMetadata)));

        var expected = new AliasMetadata(
            null,
            "Test",
            [TypeMetadata.I32],
            listMetadata);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T2> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");

        var listMetadata = new TypeMetadata(null, "List", [TypeMetadata.I32], [], [], [], [], []);
        listMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                listMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], listMetadata)));

        var expected = new AliasMetadata(
            null,
            "Test",
            [TypeMetadata.I32],
            listMetadata);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeWithReserveArgumentsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<X1, X2> {}
            public type Test<T1, T2> = List<T2, T1>;

            public func(x: Test<i32, bool>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Test<i32>");

        var listMetadata = new TypeMetadata(null, "List", [TypeMetadata.I32], [], [], [], [], []);
        listMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                listMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], listMetadata)));

        var expected = new AliasMetadata(
            null,
            "Test",
            [TypeMetadata.I32],
            listMetadata);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedAliasOnAliasTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Alias2<i32>");
        var expected = new AliasMetadata(
            null,
            "Alias2",
            [TypeMetadata.I32],
            new AliasMetadata(
                null,
                "Alias1",
                [TypeMetadata.I32],
                new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, TypeMetadata.I32])));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedAliasOnAliasTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Alias1<T2> = T2 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Alias2<i32>");
        var expected = new AliasMetadata(
            null,
            "Alias2",
            [TypeMetadata.I32],
            new AliasMetadata(
                null,
                "Alias1",
                [TypeMetadata.I32],
                new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, TypeMetadata.I32])));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedAliasOnAliasTest2()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var type = typeProvider.GetType("Alias2<i32>");
        var expected = new AliasMetadata(
            null,
            "Alias2",
            [TypeMetadata.I32],
            new AliasMetadata(
                null,
                "Alias1",
                [TypeMetadata.I32],
                new DiscriminatedUnionMetadata(null, [TypeMetadata.I32, TypeMetadata.I32])));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void { }

            public test(x: i32): void { }

            public test(x: bool): void { }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var functions = semanticTree.Where<FunctionDeclaration>().ToArray();
        var functionGroup = new FunctionGroupMetadata();
        var function1 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            functionGroup);
        var function2 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var function3 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", TypeMetadata.Bool)],
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions[0].Metadata, Is.EqualTo(function1));
        Assert.That(functions[1].Metadata, Is.EqualTo(function2));
        Assert.That(functions[2].Metadata, Is.EqualTo(function3));
    }

    [Test]
    public void GenerateMetadataForDuplicateFunctionsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void { }

            public test(): void { }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(25, 3, 1), new SourcePosition(48, 3, 24))),
            "The 'test' function is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForMethodOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method(): void { }

                public method(x: i32): void { }

                public method(x: bool): void { }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var methods = semanticTree.Where<MethodDeclaration>().ToArray();
        var type = new TypeMetadata(null, "Test");
        var functionGroup = new FunctionGroupMetadata();
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            functionGroup);
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var method3 = new MethodMetadata(
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
        type.AddMethod(method3);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(methods[0].Metadata, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(methods[1].Metadata, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(methods[2].Metadata, Is.EqualTo(method3).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuplicateMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method(): void { }

                public method(): void { }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(54, 4, 5), new SourcePosition(79, 4, 30))),
            "The 'method' method is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForInterfaceMethodOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                method(): void;

                method(i32): void;

                method(bool): void;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var methods = semanticTree.Where<InterfaceMethod>().ToArray();
        var type = new InterfaceMetadata(null);
        var functionGroup = new FunctionGroupMetadata();
        var method1 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            functionGroup);
        var method2 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.Void),
            functionGroup);
        var method3 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            new FunctionTypeMetadata(null, [TypeMetadata.Bool], TypeMetadata.Void),
            functionGroup);
        type.AddMethod(method1);
        type.AddMethod(method2);
        type.AddMethod(method3);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(methods[0].Metadata, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(methods[1].Metadata, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(methods[2].Metadata, Is.EqualTo(method3).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuplicateInterfaceMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                method(): void;

                method(): void;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(46, 4, 5), new SourcePosition(61, 4, 20))),
            "The 'method' method is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForVariableWithAnonymousTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var du: i32 | null = (i32 | null)1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var expectedDu = new DiscriminatedUnionMetadata(null, [
            TypeMetadata.I32,
            TypeMetadata.Null,
        ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualDu = typeProvider.GetType("i32 | null");
        Assert.That(actualDu, Is.EqualTo(expectedDu).Using(new MetadataComparer()));
    }
}