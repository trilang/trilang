using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class MetadataGeneratorTests
{
    [Test]
    public void GenerateMetadataForTypeTest()
    {
        var file = CreateFile(
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
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expected = new TypeMetadata(null, "Point")
        {
            Namespace = expectedTest1,
        };

        var xProperty = CreatePropertyMetadata(
            expectedRoot,
            expected,
            "x",
            compilationContext.BuiltInTypes.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = CreatePropertyMetadata(
            expectedRoot,
            expected,
            "y",
            compilationContext.BuiltInTypes.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddConstructor(new ConstructorMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            [
                new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32),
                new ParameterMetadata(null, "y", compilationContext.BuiltInTypes.I32)
            ],
            CreateFunctionType([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, expectedRoot)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            CreateFunctionType([], compilationContext.BuiltInTypes.String, expectedRoot)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Private,
            false,
            "distance",
            [new ParameterMetadata(null, "other", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.I32, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actual = test1Ns.FindType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));

        var toStringType = compilationContext.RootNamespace.FindType("() => string");
        var expectedToStringType = CreateFunctionType([], compilationContext.BuiltInTypes.String, expectedRoot);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType).Using(new MetadataComparer()));

        var distanceType = compilationContext.RootNamespace.FindType("(i32) => i32");
        var expectedDistanceType = CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.I32, expectedRoot);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                x: i32 { public get; public set; }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedBuiltInTypes = new BuiltInTypes();
        var expectedRoot = RootNamespaceMetadata.Create(expectedBuiltInTypes);
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var typeMetadata = new TypeMetadata(
            null,
            "Test",
            [],
            [],
            [],
            [],
            [],
            [],
            false,
            false)
        {
            Namespace = expectedTest1,
        };
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], expectedBuiltInTypes.Void, expectedRoot)));
        var propertyMetadata = CreatePropertyMetadata(
            expectedRoot,
            typeMetadata,
            "x",
            expectedBuiltInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);
        typeMetadata.AddMethod(propertyMetadata.Getter!);
        typeMetadata.AddMethod(propertyMetadata.Setter!);

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actual = test1Ns.FindType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata).Using(new MetadataComparer()));

        var actualProperty = (PropertyMetadata)actual.GetProperties("x")[0];
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = { }
            public type Interface2 = { }
            public type Point : Interface1, Interface2 { }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var package = NamespaceMetadata.CreateForPackage();
        var test1Namespace = package.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns)));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actual = test1Ns.FindType("Point");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingPropertyTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: xxx;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public distance(other: xxx): f64 {
                    return;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public toString(): xxx {
                    return;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type MyInt = i32;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expected = new AliasMetadata(null, "MyInt", [], compilationContext.BuiltInTypes.I32, false)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actual = test1Ns.FindType("MyInt");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyInt = xxx;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(arr: i32[]): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateArrayMetadata(compilationContext.BuiltInTypes.I32, ns);
        var actual = compilationContext.RootNamespace.FindType("i32[]");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(arr: xxx): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32, b: i32): i32 {
                return 1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateFunctionType([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32],
            compilationContext.BuiltInTypes.I32,
            ns);
        var actual = compilationContext.RootNamespace.FindType("(i32, i32) => i32");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: xxx): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(): xxx {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {}
            public type MyPoint = Point;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var package = NamespaceMetadata.CreateForPackage();
        var test1Namespace = package.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType, false)
        {
            Namespace = test1Namespace,
        };

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualType = test1Ns.FindType("Point");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyPoint = Point;
            public type Point {}
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedType, false)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualType = test1Ns.FindType("Point");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {}
            public type MyPoint = Point[];
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var package = NamespaceMetadata.CreateForPackage();
        var test1Namespace = package.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns)));

        var expectedArrayType = CreateArrayMetadata(expectedType, ns);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType, false)
        {
            Namespace = test1Namespace,
        };
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualType = test1Ns.FindType("Point");
        var actualArrayType = compilationContext.RootNamespace.FindType("Point[]");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndArrayTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyPoint = Point[];
            public type Point {}
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        var expectedArrayType = CreateArrayMetadata(expectedType, expectedRoot);
        var expectedAlias = new AliasMetadata(null, "MyPoint", [], expectedArrayType, false)
        {
            Namespace = expectedTest1,
        };

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualType = test1Ns.FindType("Point");
        var actualArrayType = compilationContext.RootNamespace.FindType("Point[]");
        var actualAlias = test1Ns.FindType("MyPoint");

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForInterfaceType()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expectedInterface = new InterfaceMetadata(null)
        {
            Namespace = expectedRoot,
        };
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "x",
                compilationContext.BuiltInTypes.I32,
                AccessModifierMetadata.Public,
                null));
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expectedInterface,
                "y",
                compilationContext.BuiltInTypes.I32,
                AccessModifierMetadata.Public,
                null));

        var expectedAlias = new AliasMetadata(null, "Point", [], expectedInterface, false)
        {
            Namespace = expectedTest1,
        };
        expectedInterface.AddMethod(new InterfaceMethodMetadata(
            null,
            expectedInterface,
            "distance",
            CreateFunctionType([expectedAlias], compilationContext.BuiltInTypes.F64, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualInterface = compilationContext.RootNamespace.FindType(expectedInterface.ToString());
        Assert.That(actualInterface, Is.EqualTo(expectedInterface).Using(new MetadataComparer()));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type DU = {} | i32 | () => void;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [
            new InterfaceMetadata(null)
            {
                Namespace = expectedRoot,
            },
            compilationContext.BuiltInTypes.I32,
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot),
        ])
        {
            Namespace = expectedRoot,
        };
        var alias = new AliasMetadata(null, "DU", [], du, false)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualDu = compilationContext.RootNamespace.FindType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Tuple = (i32, f64);
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var tuple = CreateTupleMetadata([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.F64], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple, false)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = compilationContext.RootNamespace.FindType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Tuple = (i32, (f64, bool));
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var nestedTuple = CreateTupleMetadata([compilationContext.BuiltInTypes.F64, compilationContext.BuiltInTypes.Bool], expectedRoot);
        var tuple = CreateTupleMetadata([compilationContext.BuiltInTypes.I32, nestedTuple], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple, false)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = compilationContext.RootNamespace.FindType("(i32, (f64, bool))");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualNestedTuple = compilationContext.RootNamespace.FindType("(f64, bool)");
        Assert.That(actualNestedTuple, Is.EqualTo(nestedTuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuInTupleTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Tuple = (i32, bool | i8);
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.Bool, compilationContext.BuiltInTypes.I8])
        {
            Namespace = expectedRoot,
        };
        var tuple = CreateTupleMetadata([compilationContext.BuiltInTypes.I32, du], expectedRoot);
        var alias = new AliasMetadata(null, "Tuple", [], tuple, false)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = compilationContext.RootNamespace.FindType("(i32, bool | i8)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = compilationContext.RootNamespace.FindType("bool | i8");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleInDuTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Tuple = i32 | (f64, bool);
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var tuple = CreateTupleMetadata([compilationContext.BuiltInTypes.F64, compilationContext.BuiltInTypes.Bool], expectedRoot);
        var du = new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.I32, tuple])
        {
            Namespace = expectedRoot,
        };
        var alias = new AliasMetadata(null, "Tuple", [], du, false)
        {
            Namespace = expectedTest1,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actualAlias = test1Ns.FindType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = compilationContext.RootNamespace.FindType("(f64, bool)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));

        var actualDu = compilationContext.RootNamespace.FindType("i32 | (f64, bool)");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T1, T2> {}
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var actual = test1Ns.FindType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                x: T;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test1<T> {}
            public type Test2 {
                x: T;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                x: T[];
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = test1Ns.FindType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = (PropertyMetadata)type.GetProperties("x")[0];
        Assert.That(property, Is.Not.Null);

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var typeArrayMetadata = CreateArrayMetadata(new TypeArgumentMetadata(null, "T"), expectedRoot);
        Assert.That(property.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {}
            public type Test = List<i32>;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var closedType = compilationContext.RootNamespace.FindType("List<i32>") as GenericApplicationMetadata;

        var expectedBuiltInTypes = new BuiltInTypes();
        var expectedRoot = RootNamespaceMetadata.Create(expectedBuiltInTypes);
        var expected = new TypeMetadata(
            null,
            "List",
            [expectedBuiltInTypes.I32],
            [],
            [],
            [],
            [],
            [],
            false,
            true)
        {
            Namespace = expectedRoot,
        };
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(closedType!.ClosedGeneric, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                Prop: T;
            }
            public type Test = List<i32>;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var generic = compilationContext.RootNamespace.FindType("List<i32>") as GenericApplicationMetadata;
        var closedType = generic!.ClosedGeneric;
        var property = closedType!.GetMember("Prop") as PropertyMetadata;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateDefaultCtorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {}
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
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
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                prop: T[];
            }

            public test(a: List<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var generic = (GenericApplicationMetadata)compilationContext.RootNamespace.FindType("List<i32>")!;
        var type = generic.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateArrayMetadata(compilationContext.BuiltInTypes.I32, ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericTupleTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                prop: (T, i32);
            }

            public test(a: List<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = (GenericApplicationMetadata)compilationContext.RootNamespace.FindType("List<i32>")!;
        var type = generic.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateTupleMetadata([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32], ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericDuTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                prop: T | i32;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = (GenericApplicationMetadata)compilationContext.RootNamespace.FindType("List<i32>")!;
        var type = generic.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32])
        {
            Namespace = ns,
        };

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                prop: () => T;
            }

            public test(a: List<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var generic = (GenericApplicationMetadata)compilationContext.RootNamespace.FindType("List<i32>")!;
        var type = generic.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateFunctionType([], compilationContext.BuiltInTypes.I32, ns);

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericInterfaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {
                prop: { x: T; };
            }

            public test(a: List<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var generic = (GenericApplicationMetadata)compilationContext.RootNamespace.FindType("List<i32>")!;
        var type = generic.ClosedGeneric;
        var property = type!.GetMember("prop") as PropertyMetadata;
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = new InterfaceMetadata(null)
        {
            Namespace = ns,
        };
        expected.AddProperty(
            new InterfacePropertyMetadata(
                null,
                expected,
                "x",
                compilationContext.BuiltInTypes.I32,
                AccessModifierMetadata.Public,
                null));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateHighOrderFunctionTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: ((i32) => void) => void): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("((i32) => void) => void");
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expected = CreateFunctionType(
            [CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, ns)],
            compilationContext.BuiltInTypes.Void,
            ns);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericAliasToDiscriminatedUnionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = i32 | T;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = test1Ns.FindType("Test<>");
        var typeArgumentMetadata = new TypeArgumentMetadata(null, "T");
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var package = NamespaceMetadata.CreateForPackage();
        var test1Namespace = package.CreateChild(["Test1"]);
        var du = new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.I32, typeArgumentMetadata])
        {
            Namespace = ns,
        };
        var expected = new AliasMetadata(null, "Test", [typeArgumentMetadata], du, false)
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = i32 | T;

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.I32, new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = expectedRoot,
                },
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32],
                new DiscriminatedUnionMetadata(null, [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32])
                {
                    Namespace = expectedRoot,
                },
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToFunctionTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = () => T;

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                CreateFunctionType([], new TypeArgumentMetadata(null, "T"), expectedRoot),
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32],
                CreateFunctionType([], compilationContext.BuiltInTypes.I32, expectedRoot),
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToInterfaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = { x: T; }

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
        var openAlias = new AliasMetadata(null, "Test", [new TypeArgumentMetadata(null, "T")], openInterface, false)
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
            compilationContext.BuiltInTypes.I32,
            AccessModifierMetadata.Public,
            null));
        var closedAlias = new AliasMetadata(null, "Test", [compilationContext.BuiltInTypes.I32], closedInterface, true)
        {
            Namespace = expectedRoot,
        };

        var expected = new GenericApplicationMetadata(null, openAlias, [compilationContext.BuiltInTypes.I32])
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = (i32, T);

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                CreateTupleMetadata([compilationContext.BuiltInTypes.I32, new TypeArgumentMetadata(null, "T")], expectedRoot),
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32],
                CreateTupleMetadata([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32], expectedRoot),
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToArrayTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> = T[];

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var actual = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var typeArgumentMetadata = new TypeArgumentMetadata(null, "T");
        var openGeneric = new AliasMetadata(
            null,
            "Test",
            [typeArgumentMetadata],
            CreateArrayMetadata(typeArgumentMetadata, expectedRoot),
            false
        )
        {
            Namespace = expectedTest1,
        };
        var closedGeneric = new AliasMetadata(
            null,
            "Test",
            [compilationContext.BuiltInTypes.I32],
            CreateArrayMetadata(compilationContext.BuiltInTypes.I32, expectedRoot),
            true
        )
        {
            Namespace = expectedRoot,
        };
        var expected = new GenericApplicationMetadata(null, openGeneric, [compilationContext.BuiltInTypes.I32])
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
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var nsRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var nsPackage = NamespaceMetadata.CreateForPackage();
        var nsTest1 = nsPackage.CreateChild(["Test1"]);
        var openList = new TypeMetadata(null, "List", [new TypeArgumentMetadata(null, "T")], [], [], [], [], [], false, false)
        {
            Namespace = nsTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, nsRoot)));

        var closedList = new TypeMetadata(null, "List", [compilationContext.BuiltInTypes.I32], [], [], [], [], [], false, true)
        {
            Namespace = nsRoot,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, nsRoot)));

        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new GenericApplicationMetadata(null, openList, [new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = nsRoot,
                },
                false
            )
            {
                Namespace = nsTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = nsRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32],
                new GenericApplicationMetadata(null, openList, [compilationContext.BuiltInTypes.I32])
                {
                    Namespace = nsRoot,
                    ClosedGeneric = closedList,
                },
                true
            )
            {
                Namespace = nsRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T2> {}
            public type Test<T> = List<T>;

            public func(x: Test<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var openList = new TypeMetadata(
            null,
            "List",
            [new TypeArgumentMetadata(null, "T2")],
            [],
            [],
            [],
            [],
            [],
            false,
            false)
        {
            Namespace = expectedTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        var closedList = new TypeMetadata(
            null,
            "List",
            [compilationContext.BuiltInTypes.I32],
            [],
            [],
            [],
            [],
            [],
            false,
            true)
        {
            Namespace = expectedRoot,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        var expected = new GenericApplicationMetadata(
            null,
            new AliasMetadata(
                null,
                "Test",
                [new TypeArgumentMetadata(null, "T")],
                new GenericApplicationMetadata(null, openList, [new TypeArgumentMetadata(null, "T")])
                {
                    Namespace = expectedRoot,
                },
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32],
                new GenericApplicationMetadata(null, openList, [compilationContext.BuiltInTypes.I32])
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = closedList,
                },
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedGenericAliasToTypeWithReserveArgumentsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<X1, X2> {}
            public type Test<T1, T2> = List<T2, T1>;

            public func(x: Test<i32, bool>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Test<i32, bool>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
        var openList = new TypeMetadata(
            null,
            "List",
            [new TypeArgumentMetadata(null, "X1"), new TypeArgumentMetadata(null, "X2")],
            [],
            [],
            [],
            [],
            [],
            false,
            false)
        {
            Namespace = expectedTest1,
        };
        openList.AddConstructor(
            new ConstructorMetadata(
                null,
                openList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

        var closedList = new TypeMetadata(
            null,
            "List",
            [compilationContext.BuiltInTypes.Bool, compilationContext.BuiltInTypes.I32],
            [],
            [],
            [],
            [],
            [],
            false,
            true)
        {
            Namespace = expectedRoot,
        };
        closedList.AddConstructor(
            new ConstructorMetadata(
                null,
                closedList,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, expectedRoot)));

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
                },
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.Bool]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Test",
                [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.Bool],
                new GenericApplicationMetadata(
                    null,
                    openList,
                    [compilationContext.BuiltInTypes.Bool, compilationContext.BuiltInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = closedList,
                },
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedAliasOnAliasTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Alias2<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                        [new TypeArgumentMetadata(null, "T1"), compilationContext.BuiltInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    },
                    false
                )
                {
                    Namespace = expectedTest1,
                },
                [new TypeArgumentMetadata(null, "T1")]
            )
            {
                Namespace = expectedRoot,
            },
            false
        )
        {
            Namespace = expectedTest1,
        };

        var closedAlias2 = new AliasMetadata(
            null,
            "Alias2",
            [compilationContext.BuiltInTypes.I32],
            new GenericApplicationMetadata(
                null,
                new AliasMetadata(
                    null,
                    "Alias1",
                    [new TypeArgumentMetadata(null, "T1")],
                    new DiscriminatedUnionMetadata(
                        null,
                        [new TypeArgumentMetadata(null, "T1"), compilationContext.BuiltInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    },
                    false
                )
                {
                    Namespace = expectedTest1,
                },
                [compilationContext.BuiltInTypes.I32]
            )
            {
                Namespace = expectedRoot,
                ClosedGeneric = new AliasMetadata(
                    null,
                    "Alias1",
                    [compilationContext.BuiltInTypes.I32],
                    new DiscriminatedUnionMetadata(
                        null,
                        [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32])
                    {
                        Namespace = expectedRoot,
                    },
                    true
                )
                {
                    Namespace = expectedRoot,
                },
            },
            true
        )
        {
            Namespace = expectedRoot,
        };

        var expected = new GenericApplicationMetadata(null, openAlias2, [compilationContext.BuiltInTypes.I32])
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Alias1<T2> = T2 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Alias2<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                            [new TypeArgumentMetadata(null, "T2"), compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        false
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [new TypeArgumentMetadata(null, "T1")]
                )
                {
                    Namespace = expectedRoot,
                },
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Alias2",
                [compilationContext.BuiltInTypes.I32],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T2")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T2"), compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        false
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [compilationContext.BuiltInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = new AliasMetadata(
                        null,
                        "Alias1",
                        [compilationContext.BuiltInTypes.I32],
                        new DiscriminatedUnionMetadata(
                            null,
                            [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        true
                    )
                    {
                        Namespace = expectedRoot,
                    }
                },
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedClosedAliasOnAliasTest2()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            public test(x: Alias2<i32>): void {
                return;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var type = compilationContext.RootNamespace.FindType("Alias2<i32>");

        var expectedRoot = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1 = expectedPackage.CreateChild(["Test1"]);
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
                            [new TypeArgumentMetadata(null, "T1"), compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        false
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [new TypeArgumentMetadata(null, "T1")]
                )
                {
                    Namespace = expectedRoot,
                },
                false
            )
            {
                Namespace = expectedTest1,
            },
            [compilationContext.BuiltInTypes.I32]
        )
        {
            Namespace = expectedRoot,
            ClosedGeneric = new AliasMetadata(
                null,
                "Alias2",
                [compilationContext.BuiltInTypes.I32],
                new GenericApplicationMetadata(
                    null,
                    new AliasMetadata(
                        null,
                        "Alias1",
                        [new TypeArgumentMetadata(null, "T1")],
                        new DiscriminatedUnionMetadata(
                            null,
                            [new TypeArgumentMetadata(null, "T1"), compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        false
                    )
                    {
                        Namespace = expectedTest1,
                    },
                    [compilationContext.BuiltInTypes.I32]
                )
                {
                    Namespace = expectedRoot,
                    ClosedGeneric = new AliasMetadata(
                        null,
                        "Alias1",
                        [compilationContext.BuiltInTypes.I32],
                        new DiscriminatedUnionMetadata(
                            null,
                            [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32]
                        )
                        {
                            Namespace = expectedRoot,
                        },
                        true
                    )
                    {
                        Namespace = expectedRoot,
                    }
                },
                true
            )
            {
                Namespace = expectedRoot,
            },
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionOverloadTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void { }

            public test(x: i32): void { }

            public test(x: bool): void { }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var functions = semanticTree.Where<FunctionDeclaration>().ToArray();
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var function1 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns));
        var function2 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, ns));
        var function3 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, ns));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(functions[0].Metadata, Is.EqualTo(function1).Using(new MetadataComparer()));
        Assert.That(functions[1].Metadata, Is.EqualTo(function2).Using(new MetadataComparer()));
        Assert.That(functions[2].Metadata, Is.EqualTo(function3).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDuplicateFunctionsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void { }

            public test(): void { }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method(): void { }

                public method(x: i32): void { }

                public method(x: bool): void { }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var methods = semanticTree.Where<MethodDeclaration>().ToArray();
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
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
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns));
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, ns));
        var method3 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, ns));
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method(): void { }

                public method(): void { }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                method(): void;

                method(i32): void;

                method(bool): void;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var methods = semanticTree.Where<InterfaceMethod>().ToArray();
        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var type = new InterfaceMetadata(null)
        {
            Namespace = ns,
        };
        var method1 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, ns));
        var method2 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, ns));
        var method3 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, ns));
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
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                method(): void;

                method(): void;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

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
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var du: i32 | null = (i32 | null)1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns = RootNamespaceMetadata.Create(new BuiltInTypes());
        var expectedDu = new DiscriminatedUnionMetadata(null, [
            compilationContext.BuiltInTypes.I32,
            compilationContext.BuiltInTypes.Null,
        ])
        {
            Namespace = ns,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var actualDu = compilationContext.RootNamespace.FindType("i32 | null");
        Assert.That(actualDu, Is.EqualTo(expectedDu).Using(new MetadataComparer()));
    }
}