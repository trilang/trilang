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
            "toString",
            new FunctionTypeMetadata("() => string")));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Private,
            "distance",
            new FunctionTypeMetadata("(i32) => i32")));

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected));

        var toStringType = typeProvider.GetType("() => string");
        var expectedToStringType = new FunctionTypeMetadata("() => string");
        Assert.That(toStringType, Is.EqualTo(expectedToStringType));

        var distanceType = typeProvider.GetType("(i32) => i32");
        var expectedDistanceType = new FunctionTypeMetadata("(i32) => i32");
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType));
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
        var propertyMetadata = new PropertyMetadata(typeMetadata, "x", TypeMetadata.I32);
        propertyMetadata.Getter = new PropertyGetterMetadata(propertyMetadata, AccessModifierMetadata.Public);
        propertyMetadata.Setter = new PropertySetterMetadata(propertyMetadata, AccessModifierMetadata.Public);
        typeMetadata.AddProperty(propertyMetadata);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Test") as TypeMetadata;
        Assert.That(actual, Is.EqualTo(typeMetadata));

        var actualProperty = actual.GetProperty("x");
        Assert.That(actualProperty, Is.Not.Null);
        Assert.That(propertyMetadata, Is.EqualTo(actualProperty));
        Assert.That(actualProperty.Getter, Is.Not.Null);
        Assert.That(propertyMetadata.Getter, Is.EqualTo(actualProperty.Getter));
        Assert.That(actualProperty.Setter, Is.Not.Null);
        Assert.That(propertyMetadata.Setter, Is.EqualTo(actualProperty.Setter));
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

        var interface1Metadata = new InterfaceMetadata("Interface1");
        var interface2Metadata = new InterfaceMetadata("Interface2");
        var expected = new TypeMetadata(
            "Point",
            [],
            [interface1Metadata, interface2Metadata],
            [],
            [],
            []);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected));
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

        var expected = new TypeAliasMetadata("MyInt", TypeMetadata.I32);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected));
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

        var expected = new TypeArrayMetadata("i32[]") { ItemMetadata = TypeMetadata.I32 };

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("i32[]");
        Assert.That(actual, Is.EqualTo(expected));
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

        var expected = new FunctionTypeMetadata("(i32, i32) => i32");

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("(i32, i32) => i32");
        Assert.That(actual, Is.EqualTo(expected));
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
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
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
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
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
        var expectedArrayType = new TypeArrayMetadata("Point[]") { ItemMetadata = expectedType };
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
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
        var expectedArrayType = new TypeArrayMetadata("Point[]") { ItemMetadata = expectedType };
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualType = typeProvider.GetType("Point");
        var actualArrayType = typeProvider.GetType("Point[]");
        var actualAlias = typeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
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
                        .DefineParameter("other", "Point")
                        .ReturnType("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedInterface = new InterfaceMetadata("{ x: i32; y: i32; distance(Point): f64; }");
        expectedInterface.AddProperty(new InterfacePropertyMetadata(expectedInterface, "x", TypeMetadata.I32));
        expectedInterface.AddProperty(new InterfacePropertyMetadata(expectedInterface, "y", TypeMetadata.I32));

        var expectedAlias = new TypeAliasMetadata("Point", expectedInterface);
        expectedInterface.AddMethod(new InterfaceMethodMetadata(
            expectedInterface,
            "distance",
            new FunctionTypeMetadata("(Point) => f64")));

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualInterface = typeProvider.GetType(expectedInterface.Name);
        Assert.That(actualInterface, Is.EqualTo(expectedInterface));

        var actualAlias = typeProvider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
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

        var du = new DiscriminatedUnionMetadata("{ } | i32 | () => void");
        du.AddType(new InterfaceMetadata("{ }", [], []));
        du.AddType(TypeMetadata.I32);
        du.AddType(new FunctionTypeMetadata("() => void"));
        var alias = new TypeAliasMetadata("DU", du);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualDu = typeProvider.GetType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du));
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

        var tuple = new TupleMetadata("(i32, f64)");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(TypeMetadata.F64);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = typeProvider.GetType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple));
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

        var nestedTuple = new TupleMetadata("(f64, bool)");
        nestedTuple.AddType(TypeMetadata.F64);
        nestedTuple.AddType(TypeMetadata.Bool);

        var tuple = new TupleMetadata("(i32, (f64, bool))");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(nestedTuple);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = typeProvider.GetType("(i32, (f64, bool))");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualNestedTuple = typeProvider.GetType("(f64, bool)");
        Assert.That(actualNestedTuple, Is.EqualTo(nestedTuple));
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

        var du = new DiscriminatedUnionMetadata("bool | i8");
        du.AddType(TypeMetadata.Bool);
        du.AddType(TypeMetadata.I8);

        var tuple = new TupleMetadata("(i32, bool | i8)");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(du);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = typeProvider.GetType("(i32, bool | i8)");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualDu = typeProvider.GetType("bool | i8");
        Assert.That(actualDu, Is.EqualTo(du));
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

        var tuple = new TupleMetadata("(f64, bool)");
        tuple.AddType(TypeMetadata.F64);
        tuple.AddType(TypeMetadata.Bool);

        var du = new DiscriminatedUnionMetadata("i32 | (f64, bool)");
        du.AddType(TypeMetadata.I32);
        du.AddType(tuple);

        var alias = new TypeAliasMetadata("Tuple", du);

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actualAlias = typeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = typeProvider.GetType("(f64, bool)");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualDu = typeProvider.GetType("i32 | (f64, bool)");
        Assert.That(actualDu, Is.EqualTo(du));
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

        var expected = new TypeMetadata("Test<,>");
        expected.AddGenericArgument(new TypeArgumentMetadata("T1"));
        expected.AddGenericArgument(new TypeArgumentMetadata("T2"));

        var typeProvider = tree.SymbolTable!.TypeProvider;
        var actual = typeProvider.GetType("Test<,>");
        Assert.That(actual, Is.EqualTo(expected));
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
        Assert.That(property.Type, Is.EqualTo(new TypeArgumentMetadata("T")));
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

        var typeArrayMetadata = new TypeArrayMetadata("T[]")
        {
            ItemMetadata = new TypeArgumentMetadata("T")
        };
        Assert.That(property.Type, Is.EqualTo(typeArrayMetadata));
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

        var expected = new TypeMetadata("List<i32>");
        expected.AddGenericArgument(TypeMetadata.I32);

        Assert.That(closedType, Is.EqualTo(expected));
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
        Assert.That(property.Type, Is.EqualTo(TypeMetadata.I32));
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
        Assert.That(property.Type, Is.EqualTo(new TypeArrayMetadata("i32[]")));
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
        Assert.That(property.Type, Is.EqualTo(new TupleMetadata("(i32, i32)")));
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
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Type, Is.EqualTo(new DiscriminatedUnionMetadata("i32 | i32")));
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
        Assert.That(property.Type, Is.EqualTo(new FunctionTypeMetadata("() => i32")));
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
        Assert.That(property.Type, Is.EqualTo(new InterfaceMetadata("{ x: i32; }")));
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
        var expected = new FunctionTypeMetadata("((i32) => void) => void");

        Assert.That(type, Is.Not.Null);
        Assert.That(type, Is.EqualTo(expected));
    }
}