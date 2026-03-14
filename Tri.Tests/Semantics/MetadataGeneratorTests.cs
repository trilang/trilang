using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;

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
            namespace Test1;

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

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new TypeMetadata(null, "Point")
        {
            Namespace = expectedTest1,
        };

        var xProperty = CreatePropertyMetadata(expected,
            "x",
            builtInTypes.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = CreatePropertyMetadata(expected,
            "y",
            builtInTypes.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddConstructor(new ConstructorMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            [
                new ParameterMetadata(null, "x", builtInTypes.I32),
                new ParameterMetadata(null, "y", builtInTypes.I32)
            ],
            CreateFunctionType([builtInTypes.I32, builtInTypes.I32], builtInTypes.Void, expectedRoot)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            CreateFunctionType([], builtInTypes.String, expectedRoot)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Private,
            false,
            "distance",
            [new ParameterMetadata(null, "other", builtInTypes.I32)],
            CreateFunctionType([builtInTypes.I32], builtInTypes.I32, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actual = test1Ns.FindType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));

        var toStringType = rootNamespace.FindType("() => string");
        var expectedToStringType = CreateFunctionType([], builtInTypes.String, expectedRoot);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType).Using(new MetadataComparer()));

        var distanceType = rootNamespace.FindType("(i32) => i32");
        var expectedDistanceType = CreateFunctionType([builtInTypes.I32], builtInTypes.I32, expectedRoot);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                x: i32 { public get; public set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var builtInTypes = new BuiltInTypes();
        var expectedRoot = NamespaceMetadata.CreateRoot(builtInTypes);
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var typeMetadata = new TypeMetadata(null, "Test")
        {
            Namespace = expectedTest1,
        };
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));
        var propertyMetadata = CreatePropertyMetadata(typeMetadata,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);
        typeMetadata.AddMethod(propertyMetadata.Getter!);
        typeMetadata.AddMethod(propertyMetadata.Setter!);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actual = test1Ns.FindType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata).Using(new MetadataComparer()));

        var actualProperty = (PropertyMetadata)actual.GetProperties("x")[0];
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Interface1 = { }
            public type Interface2 = { }
            public type Point : Interface1, Interface2 { }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var test1Namespace = ns.CreateChild(["Test1"]);
        var interfaceMetadata = new InterfaceMetadata(null)
        {
            Namespace = ns,
        };
        var expected = new TypeMetadata(
            null,
            "Point",
            [],
            [interfaceMetadata],
            [],
            [],
            [],
            [])
        {
            Namespace = test1Namespace,
        };
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, ns)));

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actual = test1Ns.FindType("Point");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingPropertyTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: xxx;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(45, 4, 8), new SourcePosition(48, 4, 11))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public distance(other: xxx): f64 {
                    return;
                }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0003UnknownType,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(65, 4, 28), new SourcePosition(68, 4, 31))),
                "Unknown type: 'xxx'."),
            new Diagnostic(
                DiagnosticId.S0004ReturnTypeMismatch,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(85, 5, 9), new SourcePosition(92, 5, 16))),
                "Return type mismatch: expected 'f64', got 'void'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): xxx {
                    return;
                }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(61, 4, 24), new SourcePosition(64, 4, 27))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type MyInt = i32;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new AliasMetadata(null, "MyInt", [], builtInTypes.I32)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actual = test1Ns.FindType("MyInt");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type MyInt = xxx;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(38, 3, 21), new SourcePosition(41, 3, 24))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(arr: i32[]): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateArrayMetadata(builtInTypes.I32, ns);
        var actual = rootNamespace.FindType("i32[]");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(arr: xxx): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(35, 3, 18), new SourcePosition(38, 3, 21))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(a: i32, b: i32): i32 {
                return 1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateFunctionType([builtInTypes.I32, builtInTypes.I32],
            builtInTypes.I32,
            ns);
        var actual = rootNamespace.FindType("(i32, i32) => i32");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(a: xxx): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(33, 3, 16), new SourcePosition(36, 3, 19))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): xxx {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(33, 3, 16), new SourcePosition(36, 3, 19))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {}
            public type MyPoint = Point;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var test1Namespace = ns.CreateChild(["Test1"]);
        var expectedType = new TypeMetadata(null, "Point")
        {
            Namespace = test1Namespace,
        };
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, ns)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType)
        {
            Namespace = test1Namespace,
        };

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualType = test1Ns.FindType("Point");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type MyPoint = Point;
            public type Point {}
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expectedType = new TypeMetadata(null, "Point")
        {
            Namespace = expectedTest1,
        };
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualType = test1Ns.FindType("Point");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {}
            public type MyPoint = Point[];
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var test1Namespace = ns.CreateChild(["Test1"]);
        var expectedType = new TypeMetadata(null, "Point")
        {
            Namespace = test1Namespace,
        };
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, ns)));

        var expectedArrayType = CreateArrayMetadata(expectedType, ns);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType)
        {
            Namespace = test1Namespace,
        };
        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualType = test1Ns.FindType("Point");
        var actualArrayType = rootNamespace.FindType("Point[]");
        var actualAlias = test1Ns.FindType("MyPoint");

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
            namespace Test1;

            public type MyPoint = Point[];
            public type Point {}
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expectedType = new TypeMetadata(null, "Point")
        {
            Namespace = expectedTest1,
        };
        expectedType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var expectedArrayType = CreateArrayMetadata(expectedType, expectedRoot);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualType = test1Ns.FindType("Point");
        var actualArrayType = rootNamespace.FindType("Point[]");
        var actualAlias = test1Ns.FindType("MyPoint");

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
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expectedInterface = new InterfaceMetadata(null)
        {
            Namespace = expectedRoot,
        };
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "x",
                builtInTypes.I32,
                AccessModifierMetadata.Public,
                null));
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "y",
                builtInTypes.I32,
                AccessModifierMetadata.Public,
                null));

        var expectedAlias = new AliasMetadata(null, "Point", [], expectedInterface)
        {
            Namespace = expectedTest1,
        };
        expectedInterface.AddMethod(new InterfaceMethodMetadata(
            null,
            expectedInterface,
            "distance",
            CreateFunctionType([expectedAlias], builtInTypes.F64, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualInterface = rootNamespace.FindType(expectedInterface.ToString());
        Assert.That(actualInterface, Is.EqualTo(expectedInterface).Using(new MetadataComparer()));

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type DU = {} | i32 | () => void;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [
            new InterfaceMetadata(null)
            {
                Namespace = expectedRoot,
            },
            builtInTypes.I32,
            CreateFunctionType([], builtInTypes.Void, expectedRoot),
        ])
        {
            Namespace = expectedRoot,
        };
        var alias = new AliasMetadata(null, "DU", [], du)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualDu = rootNamespace.FindType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Tuple = (i32, f64);
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var tuple = CreateTupleMetadata([builtInTypes.I32, builtInTypes.F64], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = rootNamespace.FindType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Tuple = (i32, (f64, bool));
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var nestedTuple = CreateTupleMetadata([builtInTypes.F64, builtInTypes.Bool], expectedRoot);
        var tuple = CreateTupleMetadata([builtInTypes.I32, nestedTuple], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = rootNamespace.FindType("(i32, (f64, bool))");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualNestedTuple = rootNamespace.FindType("(f64, bool)");
        Assert.That(actualNestedTuple, Is.EqualTo(nestedTuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuInTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Tuple = (i32, bool | i8);
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [builtInTypes.Bool, builtInTypes.I8])
        {
            Namespace = expectedRoot,
        };
        var tuple = CreateTupleMetadata([builtInTypes.I32, du], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = rootNamespace.FindType("(i32, bool | i8)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = rootNamespace.FindType("bool | i8");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleInDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Tuple = i32 | (f64, bool);
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var tuple = CreateTupleMetadata([builtInTypes.F64, builtInTypes.Bool], expectedRoot);
        var du = new DiscriminatedUnionMetadata(null, [builtInTypes.I32, tuple])
        {
            Namespace = expectedRoot,
        };
        var alias = new AliasMetadata(null, "Tuple", [], du)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = rootNamespace.FindType("(f64, bool)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = rootNamespace.FindType("i32 | (f64, bool)");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T1, T2> {}
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new TypeMetadata(null, "Test")
        {
            Namespace = expectedTest1,
        };
        expected.AddGenericArgument(new TypeArgumentMetadata(null, "T1"));
        expected.AddGenericArgument(new TypeArgumentMetadata(null, "T2"));
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var actual = test1Ns.FindType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> {
                x: T;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var type = test1Ns.FindType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = (PropertyMetadata)type.GetProperties("x")[0];
        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(new TypeArgumentMetadata(null, "T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyInWrongTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test1<T> {}
            public type Test2 {
                x: T;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(69, 5, 8), new SourcePosition(70, 5, 9))),
            "Unknown type: 'T'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForGenericArrayPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> {
                x: T[];
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var type = test1Ns.FindType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = (PropertyMetadata)type.GetProperties("x")[0];
        Assert.That(property, Is.Not.Null);

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var typeArrayMetadata = CreateArrayMetadata(new TypeArgumentMetadata(null, "T"), expectedRoot);
        Assert.That(property.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {}
            public type Test = List<i32>;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var closedType = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new TypeMetadata(null, "List")
        {
            Namespace = expectedTest1,
        };
        expected.AddGenericArgument(builtInTypes.I32);
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(closedType!.ClosedGeneric, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {
                Prop: T;
            }
            public type Test = List<i32>;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var closedType = generic!.ClosedGeneric;
        var property = closedType!.GetMember("Prop") as PropertyMetadata;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(builtInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateDefaultCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {}
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var type = test1Ns.FindType("Test") as TypeMetadata;
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
            namespace Test1;

            public type List<T> {
                prop: T[];
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var type = generic!.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateArrayMetadata(builtInTypes.I32, ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {
                prop: (T, i32);
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var type = generic!.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateTupleMetadata([builtInTypes.I32, builtInTypes.I32], ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {
                prop: T | i32;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var type = generic!.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = new DiscriminatedUnionMetadata(null, [builtInTypes.I32, builtInTypes.I32])
        {
            Namespace = ns,
        };

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {
                prop: () => T;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var type = generic!.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateFunctionType([], builtInTypes.I32, ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {
                prop: { x: T; };
            }

            public test(a: List<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var generic = rootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var type = generic!.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = new InterfaceMetadata(null)
        {
            Namespace = ns,
        };
        expected.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expected,
                "x",
                builtInTypes.I32,
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
            namespace Test1;

            public test(a: ((i32) => void) => void): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("((i32) => void) => void");
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expected = CreateFunctionType([
                CreateFunctionType([builtInTypes.I32], builtInTypes.Void, ns)
            ],
            builtInTypes.Void,
            ns);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericAliasToDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = i32 | T;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var type = test1Ns.FindType("Test<>");
        var typeArgumentMetadata = new TypeArgumentMetadata(null, "T");
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var test1Namespace = ns.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [builtInTypes.I32, typeArgumentMetadata])
        {
            Namespace = ns,
        };
        var expected = new AliasMetadata(null, "Test", [typeArgumentMetadata], du)
        {
            Namespace = test1Namespace,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = i32 | T;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new DiscriminatedUnionMetadata(null, [builtInTypes.I32, new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32],
                new DiscriminatedUnionMetadata(null, [builtInTypes.I32, builtInTypes.I32])
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = () => T;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = rootNamespace.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                CreateFunctionType([], new TypeArgumentMetadata(null, "T"), expectedRoot)
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32],
                CreateFunctionType([], builtInTypes.I32, expectedRoot)
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = { x: T; }

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = rootNamespace.CreateChild(["Test1"]);
        var openInterface = new InterfaceMetadata(null)
        {
            Namespace = expectedRoot,
        };
        openInterface.AddProperty(new InterfacePropertyMetadata(
            null,
            openInterface,
            "x",
            new TypeArgumentMetadata(null, "T"),
            AccessModifierMetadata.Public,
            null));
        var openAlias = new AliasMetadata(null, "Test", [new TypeArgumentMetadata(null, "T")], openInterface)
        {
            Namespace = expectedTest1,
        };

        var closedInterface = new InterfaceMetadata(null)
        {
            Namespace = expectedRoot,
        };
        closedInterface.AddProperty(new InterfacePropertyMetadata(
            null,
            closedInterface,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            null));
        var closedAlias = new AliasMetadata(null, "Test", [builtInTypes.I32], closedInterface)
        {
            Namespace = expectedTest1,
        };

        var expected = new GenericApplicationMetadata(null, openAlias, [builtInTypes.I32])
        {
            Namespace = expectedRoot,
            ClosedGeneric = closedAlias,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTupleTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = (i32, T);

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = rootNamespace.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                CreateTupleMetadata([builtInTypes.I32, new TypeArgumentMetadata(null, "T")], expectedRoot)
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32],
                CreateTupleMetadata([builtInTypes.I32, builtInTypes.I32], expectedRoot)
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test<T> = T[];

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var actual = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var typeArgumentMetadata = new TypeArgumentMetadata(null, "T");
        var openGeneric = new AliasMetadata(
            null,
            "Test",
            [typeArgumentMetadata],
            CreateArrayMetadata(typeArgumentMetadata, expectedRoot)
        )
        {
            Namespace = expectedTest1,
        };
        var closedGeneric = new AliasMetadata(
            null,
            "Test",
            [builtInTypes.I32],
            CreateArrayMetadata(builtInTypes.I32, expectedRoot)
        )
        {
            Namespace = expectedTest1,
        };
        var expected = new GenericApplicationMetadata(null, openGeneric, [builtInTypes.I32])
        {
            Namespace = expectedRoot,
            ClosedGeneric = closedGeneric,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.Not.Null);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var nsTest1 = ns.CreateChild(["Test1"]);
        var openList = new TypeMetadata(null, "List", [new TypeArgumentMetadata(null, "T")], [], [], [], [], [])
        {
            Namespace = nsTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, ns)));

        var closedList = new TypeMetadata(null, "List", [builtInTypes.I32], [], [], [], [], [])
        {
            Namespace = nsTest1,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, ns)));

        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new GenericApplicationMetadata(null, openList, [new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = ns,
                }
            )
            {
                Namespace = nsTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = ns,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32],
                new GenericApplicationMetadata(null, openList, [builtInTypes.I32])
                {
                    Namespace = ns,
                    ClosedGeneric = closedList,
                }
            )
            {
                Namespace = nsTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T2> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var openList = new TypeMetadata(null, "List", [new TypeArgumentMetadata(null, "T2")], [], [], [], [], [])
        {
            Namespace = expectedTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var closedList = new TypeMetadata(null, "List", [builtInTypes.I32], [], [], [], [], [])
        {
            Namespace = expectedTest1,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new GenericApplicationMetadata(null, openList, [new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32],
                new GenericApplicationMetadata(null, openList, [builtInTypes.I32])
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = closedList,
                }
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeWithReserveArgumentsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<X1, X2> {}
            public type Test<T1, T2> = List<T2, T1>;

            public func(x: Test<i32, bool>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Test<i32, bool>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var openList = new TypeMetadata(null, "List", [new TypeArgumentMetadata(null, "X1"), new TypeArgumentMetadata(null, "X2")], [], [], [], [], [])
        {
            Namespace = expectedTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var closedList = new TypeMetadata(null, "List", [builtInTypes.Bool, builtInTypes.I32], [], [], [], [], [])
        {
            Namespace = expectedTest1,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, expectedRoot)));

        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T1"), new TypeArgumentMetadata(null, "T2")],
                new GenericApplicationMetadata(
                    null,
                    openList,
                    [new TypeArgumentMetadata(null, "T2"), new TypeArgumentMetadata(null, "T1")]
                )
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32, builtInTypes.Bool]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [builtInTypes.I32, builtInTypes.Bool],
                new GenericApplicationMetadata(
                    null,
                    openList,
                    [builtInTypes.Bool, builtInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = closedList,
                }
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedAliasOnAliasTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Alias2<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var openAlias2 = new AliasMetadata(
            null,
            "Alias2",
            [new TypeArgumentMetadata(null, "T1")],
            new GenericApplicationMetadata(
                null,
                new AliasMetadata(
                    null,
                    "Alias1",
                    [new TypeArgumentMetadata(null, "T1")],
                    new DiscriminatedUnionMetadata(
                        null,
                        [new TypeArgumentMetadata(null, "T1"), builtInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    }
                )
                {
                    Namespace = expectedTest1,
                },
                [new TypeArgumentMetadata(null, "T1")]
            )
            {
                Namespace = expectedRoot,
            }
        )
        {
            Namespace = expectedTest1,
        };

        var closedAlias2 = new AliasMetadata(
            null,
            "Alias2",
            [builtInTypes.I32],
            new GenericApplicationMetadata(
                null,
                new AliasMetadata(
                    null,
                    "Alias1",
                    [new TypeArgumentMetadata(null, "T1")],
                    new DiscriminatedUnionMetadata(
                        null,
                        [new TypeArgumentMetadata(null, "T1"), builtInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    }
                )
                {
                    Namespace = expectedTest1,
                },
                [builtInTypes.I32]
            )
            {
                Namespace = expectedRoot,
                ClosedGeneric = new AliasMetadata(
                    null,
                    "Alias1",
                    [builtInTypes.I32],
                    new DiscriminatedUnionMetadata(
                        null,
                        [builtInTypes.I32, builtInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    }
                )
                {
                    Namespace = expectedTest1,
                },
            }
        )
        {
            Namespace = expectedTest1,
        };

        var expected = new GenericApplicationMetadata(null, openAlias2, [builtInTypes.I32])
        {
            Namespace = expectedRoot,
            ClosedGeneric = closedAlias2,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedAliasOnAliasTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Alias1<T2> = T2 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Alias2<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Alias2",
                [new TypeArgumentMetadata(null, "T1")],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T2")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T2"), builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [new TypeArgumentMetadata(null, "T1")]
                )
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Alias2",
                [builtInTypes.I32],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T2")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T2"), builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [builtInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = new AliasMetadata(
                        null,
                        "Alias1",
                        [builtInTypes.I32],
                        new DiscriminatedUnionMetadata(
                            null,
                            [builtInTypes.I32, builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    }
                }
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedAliasOnAliasTest2()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var type = rootNamespace.FindType("Alias2<i32>");

        var expectedRoot = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedTest1 = expectedRoot.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Alias2",
                [new TypeArgumentMetadata(null, "T1")],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T1")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T1"), builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [new TypeArgumentMetadata(null, "T1")]
                )
                {
                    Namespace = expectedRoot,
                }
            )
            {
                Namespace = expectedTest1,
            },
            [builtInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Alias2",
                [builtInTypes.I32],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T1")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T1"), builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [builtInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = new AliasMetadata(
                        null,
                        "Alias1",
                        [builtInTypes.I32],
                        new DiscriminatedUnionMetadata(
                            null,
                            [builtInTypes.I32, builtInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        }
                    )
                    {
                        Namespace = expectedTest1,
                    }
                }
            )
            {
                Namespace = expectedTest1,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void { }

            public test(x: i32): void { }

            public test(x: bool): void { }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var semanticTree = semanticTrees.Single();
        var functions = semanticTree.Where<FunctionDeclaration>().ToArray();
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var function1 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            CreateFunctionType([], builtInTypes.Void, ns));
        var function2 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", builtInTypes.I32)],
            CreateFunctionType([builtInTypes.I32], builtInTypes.Void, ns));
        var function3 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", builtInTypes.Bool)],
            CreateFunctionType([builtInTypes.Bool], builtInTypes.Void, ns));

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
            namespace Test1;

            public test(): void { }

            public test(): void { }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(43, 5, 1), new SourcePosition(66, 5, 24))),
            "The 'test' function is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForMethodOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                public method(): void { }

                public method(x: i32): void { }

                public method(x: bool): void { }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var semanticTree = semanticTrees.Single();
        var methods = semanticTree.Where<MethodDeclaration>().ToArray();
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var type = new TypeMetadata(null, "Test")
        {
            Namespace = ns,
        };
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [],
            CreateFunctionType([], builtInTypes.Void, ns));
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", builtInTypes.I32)],
            CreateFunctionType([builtInTypes.I32], builtInTypes.Void, ns));
        var method3 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", builtInTypes.Bool)],
            CreateFunctionType([builtInTypes.Bool], builtInTypes.Void, ns));
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
            namespace Test1;

            public type Test {
                public method(): void { }

                public method(): void { }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(72, 6, 5), new SourcePosition(97, 6, 30))),
            "The 'method' method is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForInterfaceMethodOverloadTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test = {
                method(): void;

                method(i32): void;

                method(bool): void;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var semanticTree = semanticTrees.Single();
        var methods = semanticTree.Where<InterfaceMethod>().ToArray();
        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var type = new InterfaceMetadata(null)
        {
            Namespace = ns,
        };
        var method1 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([], builtInTypes.Void, ns));
        var method2 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([builtInTypes.I32], builtInTypes.Void, ns));
        var method3 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([builtInTypes.Bool], builtInTypes.Void, ns));
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
            namespace Test1;

            public type Test = {
                method(): void;

                method(): void;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(64, 6, 5), new SourcePosition(79, 6, 20))),
            "The 'method' method is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForVariableWithAnonymousTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var du: i32 | null = (i32 | null)1;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns = NamespaceMetadata.CreateRoot(new BuiltInTypes());
        var expectedDu = new DiscriminatedUnionMetadata(null, [
            builtInTypes.I32,
            builtInTypes.Null,
        ])
        {
            Namespace = ns,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualDu = rootNamespace.FindType("i32 | null");
        Assert.That(actualDu, Is.EqualTo(expectedDu).Using(new MetadataComparer()));
    }
}