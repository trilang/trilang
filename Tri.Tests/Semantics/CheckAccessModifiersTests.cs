using Trilang;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class CheckAccessModifiersTests
{
    [Test]
    [TestCase(AccessModifierMetadata.Public)]
    [TestCase(AccessModifierMetadata.Internal)]
    [TestCase(AccessModifierMetadata.Private)]
    public void CtorInTheSameTypeTest(AccessModifierMetadata accessModifier)
    {
        var file = CreateFile(
            $$"""
              namespace Test1;

              public type Point {
                  {{accessModifier.ToString().ToLower()}} constructor() { }

                  public static create(): Point* {
                      return new Point();
                  }
              }
              """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PublicCtorAcrossTypesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor() { }
            }

            public type Test {
                public test(): Point* {
                    return new Point();
                }
            }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalCtorAcrossTypesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                internal constructor() { }
            }

            public type Test {
                public test(): Point* {
                    return new Point();
                }
            }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateCtorAcrossTypesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                private constructor() { }
            }

            public type Test {
                public test(): Point* {
                    return new Point();
                }
            }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(133, 9, 16), new SourcePosition(144, 9, 27))),
            "The constructor of 'Point' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PublicTypePublicCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        public constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PublicTypeInternalCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        internal constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
            "The constructor of 'Point' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PublicTypePrivateCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        private constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
            "The constructor of 'Point' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void InternalTypePublicCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Point {
                        public constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void InternalTypeInternalCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Point {
                        internal constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void InternalTypePrivateCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Point {
                        private constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void PrivateTypePublicCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    private type Point {
                        public constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void PrivateTypeInternalCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    private type Point {
                        internal constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void PrivateTypePrivateCtorAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    private type Point {
                        private constructor() { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(): Point* {
                            return new Point();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(75, 6, 20), new SourcePosition(80, 6, 25))),
                "The 'Point' type is not accessible."),
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(99, 7, 16), new SourcePosition(110, 7, 27))),
                "The 'Point' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void MissingGetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { public set; }
            }

            public test(p: Point): i32 {
                return p.x;
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(108, 8, 12), new SourcePosition(111, 8, 15))),
            "The 'x' property doesn't have a getter.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void MissingSetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { public get; }
            }

            public test(p: Point): void {
                p.x = 1;
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(102, 8, 5), new SourcePosition(105, 8, 8))),
            "The 'x' property doesn't have a setter.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateGetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { private get; private set; }
            }

            public test(): i32 {
                var p: Point* = new Point();

                return p.x;
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(148, 10, 12), new SourcePosition(151, 10, 15))),
            "The getter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateSetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { private get; private set; }
            }

            public test(): void {
                var p: Point* = new Point();

                p.x = 1;
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(142, 10, 5), new SourcePosition(145, 10, 8))),
            "The setter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateGetterInTheSameTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { private get; private set; }

                public getX(): i32 {
                    return x;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                project,
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void PrivateSetterInTheSameTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32 { private get; private set; }

                public constructor(x: i32) {
                    this.x = x;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                project,
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void InternalGetterInDifferentPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        x: i32 { internal get; internal set; }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(p: Point*): i32 {
                            return p.x;
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(105, 7, 16), new SourcePosition(108, 7, 19))),
            "The getter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void InternalSetterInDifferentPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        x: i32 { internal get; internal set; }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(p: Point*): void {
                            p.x = 1;
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(99, 7, 9), new SourcePosition(102, 7, 12))),
            "The setter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PublicMethodTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public toString(): string {
                    return "xxx";
                }
            }

            public test(p: Point): void {
                var s: string = p.toString();
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalMethodInDifferentPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Point {
                        internal method(): void { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public type Test {
                        public test(p: Point*): void {
                            p.method();
                        }
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(99, 7, 9), new SourcePosition(107, 7, 17))),
            "The 'method' method is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateMethodTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                private toString(): string {
                    return "xxx";
                }
            }

            public test(p: Point): void {
                var s: string = p.toString();
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(152, 10, 21), new SourcePosition(162, 10, 31))),
            "The 'toString' method is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateMethodInTheSameTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                private toString(): string {
                    return "xxx";
                }

                public test(): void {
                    var s: string = toString();
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateFunctionInTheSameFileTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            private test(): void { }

            public main(): void {
                test();
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalFunctionInDifferentPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal func1(): void { }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(): void {
                        func1();
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(63, 6, 5), new SourcePosition(68, 6, 10))),
            "The 'func1' function is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateStaticTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            private type Test {
                public static method(): void { }
            }

            public main(): void {
                Test.method();
            }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateStaticTypeInDifferentFilesTest()
    {
        var file1 = CreateFile(
            """
            namespace Test1;

            private type Test {
                public static method(): void { }
            }
            """);
        var file2 = CreateFile(
            """
            namespace Test1;

            public main(): void {
                Test.method();
            }
            """);
        var (project, diagnostics) = Parse(file1, file2);
        Semantic(diagnostics, project);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file2,
                    new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(48, 4, 9))),
                "The 'Test' type is not accessible.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void InternalStaticTypeInTheSamePackageTest()
    {
        var file1 = new SourceFile(
            "test.tri",
            """
            namespace Test1;

            internal type Test {
                public static method(): void { }
            }
            """);
        var file2 = new SourceFile(
            "test.tri",
            """
            namespace Test1;

            public main(): void {
                Test.method();
            }
            """);

        var (project, diagnostics) = Parse(file1, file2);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalStaticTypeInDifferentPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Test {
                        public static method(): void { }
                    }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(): void {
                        Test.method();
                    }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0014MemberNotAccessible,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project2.SourceFiles.First().File,
                    new SourceSpan(new SourcePosition(63, 6, 5), new SourcePosition(67, 6, 9))),
                "The 'Test' type is not accessible."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void PublicTypeRefTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Test { }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(t: Test*): void { }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalTypeRefTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            internal type Test { }

            public main(t: Test*): void { }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalTypeRefAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Test { }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(t: Test*): void { }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(52, 5, 16), new SourcePosition(56, 5, 20))),
            "The 'Test' type is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateTypeRefTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            private type Test { }

            public main(t: Test*): void { }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateTypeRefAcrossFilesTest()
    {
        var file1 = CreateFile(
            """
            namespace Test1;

            private type Test { }
            """);
        var file2 = CreateFile(
            """
            namespace Test1;

            public main(t: Test*): void { }
            """);
        var (project, diagnostics) = Parse(file1, file2);
        Semantic(diagnostics, project);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file2,
                new SourceSpan(new SourcePosition(33, 3, 16), new SourcePosition(37, 3, 20))),
            "The 'Test' type is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PublicGenericTypeTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type Test<T> { }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(t: Test<i32>*): void { }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalGenericTypeRefTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            internal type Test<T> { }

            public main(t: Test<i32>*): void { }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InternalGenericTypeRefAcrossPackagesTest()
    {
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    internal type Test<T> { }
                    """))
            ],
            []);
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    use test1::Test1;

                    public main(t: Test<i32>*): void { }
                    """))
            ],
            [project1.ToProjectInfo()]);

        var diagnostics = Parse(project1, project2);
        Semantic(diagnostics, project1, project2);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                project2.SourceFiles.First().File,
                new SourceSpan(new SourcePosition(52, 5, 16), new SourcePosition(61, 5, 25))),
            "The 'Test<>' type is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateGenericTypeRefTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            private type Test<T> { }

            public main(t: Test<i32>*): void { }
            """);
        var (project, diagnostics) = Parse(file);
        Semantic(diagnostics, project);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateGenericTypeRefAcrossFilesTest()
    {
        var file1 = CreateFile(
            """
            namespace Test1;

            private type Test<T> { }
            """);
        var file2 = CreateFile(
            """
            namespace Test1;

            public main(t: Test<i32>*): void { }
            """);
        var (project, diagnostics) = Parse(file1, file2);
        Semantic(diagnostics, project);

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file2,
                new SourceSpan(new SourcePosition(33, 3, 16), new SourcePosition(42, 3, 25))),
            "The 'Test<>' type is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}