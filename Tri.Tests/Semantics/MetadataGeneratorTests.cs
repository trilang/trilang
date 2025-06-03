using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MetadataGeneratorTests
{
    [Test]
    public void GenerateMetadataForTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineProperty("x", "i32")
                .DefineProperty("y", "i32")
                .DefineConstructor(b => b
                    .DefineParameter("x", "i32")
                    .DefineParameter("y", "i32"))
                .DefineMethod("toString", b => b
                    .ReturnType("string")
                    .Body(body => body.Return(r => r.String("hello"))))
                .DefineMethod("distance", b => b
                    .AccessModifier(AccessModifier.Private)
                    .DefineParameter("other", "i32")
                    .ReturnType("i32")
                    .Body(body => body.Return(r => r.Number(1)))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeMetadata("Point");
        expected.AddProperty(new PropertyMetadata(
            expected,
            "x",
            TypeMetadata.I32));
        expected.AddProperty(new PropertyMetadata(
            expected,
            "y",
            TypeMetadata.I32));
        expected.AddConstructor(new ConstructorMetadata(
            expected,
            AccessModifierMetadata.Public,
            [TypeMetadata.I32, TypeMetadata.I32]));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            new FunctionTypeMetadata([], TypeMetadata.String)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Private,
            false,
            "distance",
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)));

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("Test", builder => builder
                .DefineProperty("x", "i32", p => p
                    .Getter(AccessModifier.Public)
                    .Setter(AccessModifier.Public)))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeMetadata = new TypeMetadata("Test");
        var propertyMetadata = new PropertyMetadata(
            typeMetadata,
            "x",
            TypeMetadata.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata).Using(new MetadataComparer()));

        var actualProperty = actual.GetProperty("x");
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .AddInterface("Interface1")
                .AddInterface("Interface2"))
            .DefineAliasType("Interface1", builder => builder
                .Interface())
            .DefineAliasType("Interface2", builder => builder
                .Interface())
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var interface1Metadata = new InterfaceMetadata();
        var interface2Metadata = new InterfaceMetadata();
        var expected = new TypeMetadata(
            "Point",
            [],
            [interface1Metadata, interface2Metadata],
            [],
            [],
            []);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeMissingPropertyTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineProperty("x", "xxx"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("distance", b => b
                    .DefineParameter("other", "xxx")
                    .ReturnType("f64")
                    .Body(body => body.Return())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'other' parameter has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("toString", b => b
                    .ReturnType("xxx")
                    .Body(body => body.Return())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", t => t.Type("i32"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeAliasMetadata("MyInt", [], TypeMetadata.I32);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", t => t.Type("xxx"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'xxx' aliased type is not defined."));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", t => t.Array("i32"))
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeArrayMetadata(TypeMetadata.I32);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("i32[]");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", t => t.Type("xxx"))
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown parameter type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", t => t.Type("i32"))
                .DefineParameter("b", t => t.Type("i32"))
                .ReturnType("i32")
                .Body(body => body.Return(r => r.Number(1))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("(i32, i32) => i32");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", t => t.Type("xxx"))
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown parameter type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .ReturnType("xxx")
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", t => t.Type("Point"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", t => t.Type("Point"))
            .DefineType("Point")
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType).Using(new MetadataComparer()));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", t => t.Array("Point"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedArrayType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", t => t.Array("Point"))
            .DefineType("Point")
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", [], expectedArrayType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineProperty("y", "i32")
                    .DefineMethod("distance", m => m
                        .DefineParameter("Point")
                        .ReturnType("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

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

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualInterface = typeProvider.GetType(expectedInterface.ToString());
        Assert.That(actualInterface, Is.EqualTo(expectedInterface).Using(new MetadataComparer()));

        var actualAlias = typeProvider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("DU", builder => builder
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Interface())
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.FunctionType(f => f.ReturnType("void")))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var du = new DiscriminatedUnionMetadata([
            new InterfaceMetadata(),
            TypeMetadata.I32,
            new FunctionTypeMetadata([], TypeMetadata.Void),
        ]);
        var alias = new TypeAliasMetadata("DU", [], du);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualDu = typeProvider.GetType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Type("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var tuple = new TupleMetadata([TypeMetadata.I32, TypeMetadata.F64]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias).Using(new MetadataComparer()));

        var actualTuple = typeProvider.GetType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Tuple(t2 => t2
                        .AddCase(c2 => c2.Type("f64"))
                        .AddCase(c2 => c2.Type("bool"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var nestedTuple = new TupleMetadata([TypeMetadata.F64, TypeMetadata.Bool]);
        var tuple = new TupleMetadata([TypeMetadata.I32, nestedTuple]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.DiscriminatedUnion(du => du
                        .AddCase(c2 => c2.Type("bool"))
                        .AddCase(c2 => c2.Type("i8"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var du = new DiscriminatedUnionMetadata([TypeMetadata.Bool, TypeMetadata.I8]);
        var tuple = new TupleMetadata([TypeMetadata.I32, du]);
        var alias = new TypeAliasMetadata("Tuple", [], tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Tuple(t => t
                        .AddCase(c2 => c2.Type("f64"))
                        .AddCase(c2 => c2.Type("bool"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var tuple = new TupleMetadata([TypeMetadata.F64, TypeMetadata.Bool]);
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, tuple]);
        var alias = new TypeAliasMetadata("Tuple", [], du);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineGenericArgument("T1")
                .DefineGenericArgument("T2"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeMetadata("Test");
        expected.AddGenericArgument(new TypeArgumentMetadata("T1"));
        expected.AddGenericArgument(new TypeArgumentMetadata("T2"));

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineGenericArgument("T")
                .DefineProperty("x", "T"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<>") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var property = type.GetProperty("x");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new TypeArgumentMetadata("T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForGenericPropertyInWrongTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test1", t => t
                .DefineGenericArgument("T"))
            .DefineType("Test2", t => t
                .DefineProperty("x", "T"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property has unknown type: 'T'."));
    }

    [Test]
    public void GenerateMetadataForGenericArrayPropertyTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineGenericArgument("T")
                .DefineProperty("x", pt => pt.Array("T")))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T"))
            .DefineAliasType("Test", t => t
                .Generic("List", g => g
                    .DefineGenericArgument("i32")))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var closedType = typeProvider.GetType("List<i32>");

        var expected = new TypeMetadata("List");
        expected.AddGenericArgument(TypeMetadata.I32);

        Assert.That(closedType, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericPropertyTest()
    {
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("Prop", "T"))
            .DefineAliasType("Test", t => t
                .Generic("List", g => g
                    .DefineGenericArgument("i32")))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var closedType = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = closedType!.GetProperty("Prop");

        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateDefaultCtorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test")
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("prop", pt => pt
                    .Array("T")))
            .DefineFunction("test", f => f
                .DefineParameter("a", p => p
                    .Generic("List", g => g
                        .DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new TypeArrayMetadata(TypeMetadata.I32)).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericTupleTest()
    {
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("prop", pt => pt
                    .Tuple(tuple => tuple
                        .AddCase(c => c.Type("T"))
                        .AddCase(c => c.Type("i32")))))
            .DefineFunction("test", f => f
                .DefineParameter("a", p => p
                    .Generic("List", g => g
                        .DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("prop", pt => pt
                    .DiscriminatedUnion(du => du
                        .AddCase(c => c.Type("T"))
                        .AddCase(c => c.Type("i32")))))
            .DefineFunction("test", f => f
                .DefineParameter("a", p => p
                    .Generic("List", g => g
                        .DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        var expected = new DiscriminatedUnionMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateInlineClosedGenericFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("prop", pt => pt
                    .FunctionType(f => f.ReturnType("T"))))
            .DefineFunction("test", f => f
                .DefineParameter("a", p => p
                    .Generic("List", g => g
                        .DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T")
                .DefineProperty("prop", pt => pt
                    .Interface(i => i.DefineProperty("x", "T"))))
            .DefineFunction("test", f => f
                .DefineParameter("a", p => p
                    .Generic("List", g => g
                        .DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("List<i32>") as TypeMetadata;
        var property = type!.GetProperty("prop");
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new InterfaceMetadata()).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateHighOrderFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", f => f
                .DefineParameter("a", t => t
                    .FunctionType(ft => ft
                        .DefineParameter(p => p
                            .FunctionType(ft2 => ft2
                                .DefineParameter("i32"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Type("T"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<>");
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, new TypeArgumentMetadata("T")]);
        var expected = new TypeAliasMetadata("Test", [new TypeArgumentMetadata("T")], du);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToDiscriminatedUnionTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Type("T"))))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<i32>");
        var du = new DiscriminatedUnionMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], du);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .FunctionType(f => f.ReturnType("T")))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<i32>");
        var functionType = new FunctionTypeMetadata([], TypeMetadata.I32);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], functionType);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToInterfaceTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .Interface(i => i.DefineProperty("x", "T")))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .Tuple(tuple => tuple
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Type("T"))))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<i32>");
        var tuple = new TupleMetadata([TypeMetadata.I32, TypeMetadata.I32]);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], tuple);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .Array("T"))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<i32>");
        var array = new TypeArrayMetadata(TypeMetadata.I32);
        var expected = new TypeAliasMetadata("Test", [TypeMetadata.I32], array);

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedGenericAliasToTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("List", t => t
                .DefineGenericArgument("T"))
            .DefineAliasType("Test", t => t
                .DefineGenericArgument("T")
                .Generic("List", g => g.DefineGenericArgument("T")))
            .DefineFunction("func", f => f
                .DefineParameter("x",
                    p => p.Generic("Test", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType("Test<i32>");
        var expected = new TypeAliasMetadata(
            "Test",
            [TypeMetadata.I32],
            new TypeMetadata("List", [TypeMetadata.I32], [], [], [], []));

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenerateMetadataForClosedAliasOnAliasTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Alias1", a => a
                .DefineGenericArgument("T1")
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Type("T1"))
                    .AddCase(c => c.Type("i32"))))
            .DefineAliasType("Alias2", a => a
                .DefineGenericArgument("T1")
                .Generic("Alias1", g => g.DefineGenericArgument("T1")))
            .DefineFunction("test", f => f
                .DefineParameter("x", p => p.Generic("Alias2", g => g.DefineGenericArgument("i32"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var typeProvider = tree.SymbolTable!.TypeProvider;
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