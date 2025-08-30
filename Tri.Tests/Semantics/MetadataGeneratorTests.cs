using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MetadataGeneratorTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void GenerateMetadataForTypeTest()
    {
        var tree = Parse(
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
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeMetadata("Point");

        var xProperty = new PropertyMetadata(
            expected,
            "x",
            TypeMetadata.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = new PropertyMetadata(
            expected,
            "y",
            TypeMetadata.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddConstructor(new ConstructorMetadata(
            expected,
            AccessModifierMetadata.Public,
            [
                new ParameterMetadata("x", TypeMetadata.I32),
                new ParameterMetadata("y", TypeMetadata.I32)
            ],
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], expected)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            new FunctionTypeMetadata([], TypeMetadata.String)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Private,
            false,
            "distance",
            [new ParameterMetadata("other", TypeMetadata.I32)],
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)));

        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));

        var toStringType = typeProvider.GetType("() => string");
        var expectedToStringType = new FunctionTypeMetadata([], TypeMetadata.String);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType).Using(new MetadataComparer()));

        var distanceType = typeProvider.GetType("(i32) => i32");
        var expectedDistanceType = new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForPropertyTest()
    {
        var tree = Parse(
            """
            public type Test {
                x: i32 { public get; public set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var typeMetadata = new TypeMetadata("Test");
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], typeMetadata)));
        var propertyMetadata = new PropertyMetadata(
            typeMetadata,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);
        typeMetadata.AddMethod(propertyMetadata.Getter!);
        typeMetadata.AddMethod(propertyMetadata.Setter!);
        var actual = typeProvider.GetType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata).Using(new MetadataComparer()));

        var actualProperty = actual.GetProperty("x");
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var tree = Parse(
            """
            public type Interface1 = { }
            public type Interface2 = { }
            public type Point : Interface1, Interface2 { }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var interfaceMetadata = new InterfaceMetadata();
        var expected = new TypeMetadata(
            "Point",
            [],
            [interfaceMetadata],
            [],
            [],
            [],
            []);
        expected.AddConstructor(
            new ConstructorMetadata(
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expected)));
        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingPropertyTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: xxx;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                public distance(other: xxx): f64 {
                    return;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'other' parameter has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                public toString(): xxx {
                    return;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var tree = Parse("public type MyInt = i32;");

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeAliasMetadata("MyInt", [], TypeMetadata.I32);
        var actual = typeProvider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var tree = Parse("public type MyInt = xxx;");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'xxx' aliased type is not defined."));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var tree = Parse(
            """
            function test(arr: i32[]): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeArrayMetadata(TypeMetadata.I32);
        var actual = typeProvider.GetType("i32[]");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var tree = Parse(
            """
            function test(arr: xxx): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown parameter type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var tree = Parse(
            """
            function test(a: i32, b: i32): i32 {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        var actual = typeProvider.GetType("(i32, i32) => i32");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var tree = Parse(
            """
            function test(a: xxx): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown parameter type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var tree = Parse(
            """
            function test(): xxx {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var tree = Parse(
            """
            public type Point {}
            public type MyPoint = Point;
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expectedType = new TypeMetadata("Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expectedType)));

        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedType);
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var tree = Parse(
            """
            public type MyPoint = Point;
            public type Point {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expectedType = new TypeMetadata("Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expectedType)));

        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedType);
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var tree = Parse(
            """
            public type Point {}
            public type MyPoint = Point[];
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expectedType = new TypeMetadata("Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expectedType)));

        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedArrayType);
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndArrayTest()
    {
        var tree = Parse(
            """
            public type MyPoint = Point[];
            public type Point {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expectedType = new TypeMetadata("Point");
        expectedType.AddConstructor(
            new ConstructorMetadata(
                expectedType,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expectedType)));

        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedArrayType);
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForInterfaceType()
    {
        var tree = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expectedInterface = new InterfaceMetadata();
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                expectedInterface,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Private));
        expectedInterface.AddProperty(
            new InterfacePropertyMetadata(
                expectedInterface,
                "y",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Private));

        var expectedAlias = new TypeAliasMetadata("Point", [], expectedInterface);
        expectedInterface.AddMethod(new InterfaceMethodMetadata(
            expectedInterface,
            "distance",
            new FunctionTypeMetadata([expectedAlias], TypeMetadata.F64)));
        var actualInterface = typeProvider.GetType(expectedInterface.ToString());
        Assert.That(actualInterface, Is.EqualTo(expectedInterface).Using(new MetadataComparer()));

        var actualAlias = typeProvider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var tree = Parse(
            """
            public type DU = {} | i32 | () => void;
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var du = new DiscriminatedUnionMetadata([
            new InterfaceMetadata(),
            TypeMetadata.I32,
            new FunctionTypeMetadata([], TypeMetadata.Void),
        ]);
        var alias = new TypeAliasMetadata("DU", [], du);
        var actualAlias = typeProvider.GetType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualDu = typeProvider.GetType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var tree = Parse(
            """
            public type Tuple = (i32, f64);
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var tuple = new TupleMetadata([TypeMetadata.I32, TypeMetadata.F64]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var tree = Parse(
            """
            public type Tuple = (i32, (f64, bool));
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var nestedTuple = new TupleMetadata([TypeMetadata.F64, TypeMetadata.Bool]);
        var tuple = new TupleMetadata([TypeMetadata.I32, nestedTuple]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);

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
        var tree = Parse(
            """
            public type Tuple = (i32, bool | i8);
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var du = new DiscriminatedUnionMetadata([TypeMetadata.Bool, TypeMetadata.I8]);
        var tuple = new TupleMetadata([TypeMetadata.I32, du]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);

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
        var tree = Parse(
            """
            public type Tuple = i32 | (f64, bool);
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var tuple = new TupleMetadata([TypeMetadata.F64, TypeMetadata.Bool]);
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, tuple]);
        var alias = new TypeAliasMetadata("Tuple", [], du);

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
        var tree = Parse(
            """
            public type Test<T1, T2> {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeMetadata("Test");
        expected.AddGenericArgument(new TypeArgumentMetadata("T1"));
        expected.AddGenericArgument(new TypeArgumentMetadata("T2"));
        expected.AddConstructor(
            new ConstructorMetadata(
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expected)));

        var actual = typeProvider.GetType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                x: T;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = type.GetProperty("x");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new TypeArgumentMetadata("T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyInWrongTypeTest()
    {
        var tree = Parse(
            """
            public type Test1<T> {}
            public type Test2 {
                x: T;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property has unknown type: 'T'."));
    }

    [Test]
    public void GenerateMetadataForGenericArrayPropertyTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                x: T[];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = type.GetProperty("x");
        Assert.That(property, Is.Not.Null);

        var typeArrayMetadata = new TypeArrayMetadata(new TypeArgumentMetadata("T"));
        Assert.That(property.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericTypeTest()
    {
        var tree = Parse(
            """
            public type List<T> {}
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var closedType = typeProvider.GetType("List<i32>");

        var expected = new TypeMetadata("List");
        expected.AddGenericArgument(TypeMetadata.I32);
        expected.AddConstructor(
            new ConstructorMetadata(
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expected)));

        Assert.That(closedType, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericPropertyTest()
    {
        var tree = Parse(
            """
            public type List<T> {
                Prop: T;
            }
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var closedType = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = closedType!.GetProperty("Prop");

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateDefaultCtorTest()
    {
        var tree = Parse(
            """
            public type Test {}
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

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
        var tree = Parse(
            """
            public type List<T> {
                prop: T[];
            }

            function test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new TypeArrayMetadata(TypeMetadata.I32)).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericTupleTest()
    {
        var tree = Parse(
            """
            public type List<T> {
                prop: (T, i32);
            }

            function test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(new TupleMetadata([TypeMetadata.I32, TypeMetadata.I32])).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericDuTest()
    {
        var tree = Parse(
            """
            public type List<T> {
                prop: T | i32;
            }

            function test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        var expected = new DiscriminatedUnionMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericFunctionTest()
    {
        var tree = Parse(
            """
            public type List<T> {
                prop: () => T;
            }

            function test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(
            property.Type,
            Is.EqualTo(new FunctionTypeMetadata([], TypeMetadata.I32)).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericInterfaceTest()
    {
        var tree = Parse(
            """
            public type List<T> {
                prop: { x: T; };
            }

            function test(a: List<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        var expected = new InterfaceMetadata();
        expected.AddProperty(
            new InterfacePropertyMetadata(
                expected,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Private));

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateHighOrderFunctionTypeTest()
    {
        var tree = Parse(
            """
            function test(a: ((i32) => void) => void): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("((i32) => void) => void");
        var expected = new FunctionTypeMetadata(
            [new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.Void)],
            TypeMetadata.Void);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericAliasToDiscriminatedUnionTest()
    {
        var tree = Parse(
            """
            public type Test<T> = i32 | T;
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<>");
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, new TypeArgumentMetadata("T")]);
        var expected = new TypeAliasMetadata("Test", [new TypeArgumentMetadata("T")], du);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToDiscriminatedUnionTest()
    {
        var tree = Parse(
            """
            public type Test<T> = i32 | T;

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], du);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToFunctionTypeTest()
    {
        var tree = Parse(
            """
            public type Test<T> = () => T;

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");
        var functionType = new FunctionTypeMetadata([], TypeMetadata.I32);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], functionType);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToInterfaceTest()
    {
        var tree = Parse(
            """
            public type Test<T> = { x: T; }

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");
        var interfaceType = new InterfaceMetadata();
        interfaceType.AddProperty(new InterfacePropertyMetadata(
            interfaceType,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Private));
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], interfaceType);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTupleTest()
    {
        var tree = Parse(
            """
            public type Test<T> = (i32, T);

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");
        var tuple = new TupleMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], tuple);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToArrayTest()
    {
        var tree = Parse(
            """
            public type Test<T> = T[];

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");
        var array = new TypeArrayMetadata(TypeMetadata.I32);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], array);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTypeTest()
    {
        var tree = Parse(
            """
            public type List<T> {}
            public type Test<T> = List<T>;

            function func(x: Test<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test<i32>");

        var listMetadata = new TypeMetadata("List", [TypeMetadata.I32], [], [], [], [], []);
        listMetadata.AddConstructor(
            new ConstructorMetadata(
                listMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], listMetadata)));

        var expected = new TypeAliasMetadata(
            "Test",
            [TypeMetadata.I32],
            listMetadata);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedAliasOnAliasTest()
    {
        var tree = Parse(
            """
            public type Alias1<T1> = T1 | i32;
            public type Alias2<T1> = Alias1<T1>;

            function test(x: Alias2<i32>): void {
                return;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Alias2<i32>");
        var expected = new TypeAliasMetadata(
            "Alias2",
            [TypeMetadata.I32],
            new TypeAliasMetadata(
                "Alias1",
                [TypeMetadata.I32],
                new DiscriminatedUnionMetadata([TypeMetadata.I32, TypeMetadata.I32])));

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }
}