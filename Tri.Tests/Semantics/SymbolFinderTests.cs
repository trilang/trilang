using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class SymbolFinderTests
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
    public void FunctionWithParametersInRootScopeTest()
    {
        var (tree, diagnostics) = Parse("public add(a: i32, b: i32): void { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var a = function.Parameters[0];
        var b = function.Parameters[1];

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(
            functionBodySymbolTable.Ids,
            Contains.Item(new IdSymbol(a.Name, a)));
        Assert.That(
            functionBodySymbolTable.Ids,
            Contains.Item(new IdSymbol(b.Name, b)));
    }

    [Test]
    public void FunctionWithSameParametersInRootScopeTest()
    {
        var (tree, diagnostics) = Parse("public add(a: i32, a: i32): void { }");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(25, 1, 26))),
            "The 'a' parameter is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionWithVariablesInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                var b: i32 = 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a = variables[0];
        var b = variables[1];

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(
            functionBodySymbolTable.Ids,
            Contains.Item(new IdSymbol(a.Name, a)));
        Assert.That(
            functionBodySymbolTable.Ids,
            Contains.Item(new IdSymbol(b.Name, b)));
    }

    [Test]
    public void FunctionWithSameVariablesInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                var a: i32 = 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(46, 3, 5), new SourcePosition(61, 3, 20))),
            "The 'a' variable is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void IfScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                if (true) {
                    var a: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var a = semanticTree.Find<VariableDeclaration>()!;

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            thenSymbolTable.Ids,
            Contains.Item(new IdSymbol(a.Name, a)));
    }

    [Test]
    public void IfElseScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                if (true) {
                    var a: i32 = 1;
                } else {
                    var b: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a = variables[0];
        var b = variables[1];

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            thenSymbolTable.Ids,
            Contains.Item(new IdSymbol(a.Name, a)));

        var elseSymbolTable = map.Get(ifStatement.Else!);
        Assert.That(elseSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            elseSymbolTable.Ids,
            Contains.Item(new IdSymbol(b.Name, b)));
    }

    [Test]
    public void SameVariableInMultipleScopesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                if (true) {
                    var a: i32 = 1;
                } else {
                    var a: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a1 = variables[0];
        var a2 = variables[1];
        var a3 = variables[2];

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            functionBodySymbolTable.Ids,
            Contains.Item(new IdSymbol(a1.Name, a1)));

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            thenSymbolTable.Ids,
            Contains.Item(new IdSymbol(a2.Name, a2)));

        var elseSymbolTable = map.Get(ifStatement.Else!);
        Assert.That(elseSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(
            elseSymbolTable.Ids,
            Contains.Item(new IdSymbol(a3.Name, a3)));
    }

    [Test]
    public void TypeDeclarationDuplicateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point { }
            public type Point { }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(22, 2, 1), new SourcePosition(43, 2, 22))),
            "The 'Point' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CtorDeclarationVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor(a: i32) { }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;
        var ctor = semanticTree.Find<ConstructorDeclaration>()!;
        var parameter = ctor.Parameters[0];

        var ctorSymbolTable = map.Get(ctor.Body);
        Assert.That(ctorSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(ctorSymbolTable.Ids, Contains.Item(new IdSymbol("this", type)));
        Assert.That(
            ctorSymbolTable.Ids,
            Contains.Item(new IdSymbol(parameter.Name, parameter)));
    }

    [Test]
    public void MethodDeclarationVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public test(a: i32): void { }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;
        var method = semanticTree.Find<MethodDeclaration>()!;
        var parameter = method.Parameters[0];

        var methodSymbolTable = map.Get(method.Body);
        Assert.That(methodSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(methodSymbolTable.Ids, Contains.Item(new IdSymbol("this", type)));
        Assert.That(
            methodSymbolTable.Ids,
            Contains.Item(new IdSymbol(parameter.Name, parameter)));
    }

    [Test]
    public void TypeAliasDuplicateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type MyInt = i32;
            public type MyInt = i32;
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(25, 2, 1), new SourcePosition(49, 2, 25))),
            "The 'MyInt' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeIdsInSymbolTableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32;
                y: i32;

                public toString(): string {
                    return "pew";
                }

                public distance(other: Point): i32 {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;
        var typeSymbolTable = map.Get(type);
        Assert.That(typeSymbolTable.Ids, Has.Count.EqualTo(4));
        Assert.That(
            typeSymbolTable.Ids,
            Contains.Item(new IdSymbol("x", type.Properties[0])));
        Assert.That(
            typeSymbolTable.Ids,
            Contains.Item(new IdSymbol("y", type.Properties[1])));
        Assert.That(
            typeSymbolTable.Ids,
            Contains.Item(new IdSymbol("toString", type.Methods[0])));
        Assert.That(
            typeSymbolTable.Ids,
            Contains.Item(new IdSymbol("distance", type.Methods[1])));
    }

    [Test]
    public void FieldInGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                x: i32 {
                    private get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var semanticTree = semanticTrees.Single();
        var property = semanticTree.Find<PropertyDeclaration>()!;
        var getter = semanticTree.Find<PropertyGetter>();
        Assert.That(getter, Is.Not.Null);

        var getterSymbolTable = map.Get(getter.Body!);
        Assert.That(getterSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(getterSymbolTable.Ids, Contains.Item(new IdSymbol("field", property)));

        var setter = semanticTree.Find<PropertySetter>();
        Assert.That(setter, Is.Not.Null);

        var setterSymbolTable = map.Get(setter.Body!);
        Assert.That(setterSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(setterSymbolTable.Ids, Contains.Item(new IdSymbol("field", property)));
        Assert.That(setterSymbolTable.Ids, Contains.Item(new IdSymbol("value", property)));
    }
}