using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplacePropertyFieldAndValueWithGeneratedField : Transformer
{
    private readonly DiagnosticCollection diagnostics;
    private FieldMetadata? currentField;
    private MethodMetadata? currentSetter;

    public ReplacePropertyFieldAndValueWithGeneratedField(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        BuiltInTypes builtInTypes)
        : base(directives, builtInTypes)
    {
        this.diagnostics = diagnostics;
    }

    public override ISemanticNode TransformAlias(AliasDeclaration node)
        => node;

    public override ISemanticNode TransformArrayType(ArrayType node)
        => node;

    public override ISemanticNode TransformConstructor(ConstructorDeclaration node)
        => node;

    public override ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnion node)
        => node;

    public override ISemanticNode TransformFunction(FunctionDeclaration node)
        => node;

    public override ISemanticNode TransformFunctionType(FunctionType node)
        => node;

    public override ISemanticNode TransformGenericType(GenericApplication node)
        => node;

    public override ISemanticNode TransformGenericExpression(GenericExpression node)
        => node;

    public override ISemanticNode TransformInterface(Interface node)
        => node;

    public override ISemanticNode TransformInterfaceProperty(InterfaceProperty node)
        => node;

    public override ISemanticNode TransformInterfaceMethod(InterfaceMethod node)
        => node;

    public override ISemanticNode TransformMemberAccess(MemberAccessExpression node)
    {
        if (node.Member is not null)
            return node;

        if (node.IsField)
        {
            var metadataProvider = node.MetadataProvider!;
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics.ForSemantic(), metadataProvider);
            var pointer = metadataFactory.CreatePointer(null, currentField!.DeclaringType);
            var thisMember = new MemberAccessExpression(null, MemberAccessExpression.This)
            {
                Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                AccessKind = MemberAccessKind.Read,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };

            return new MemberAccessExpression(null, thisMember, currentField.Name)
            {
                Reference = currentField,
                AccessKind = node.AccessKind,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        if (node.IsValue)
        {
            var parameter = currentSetter!.Parameters.First(x => x.Name == MemberAccessExpression.Value);

            return new MemberAccessExpression(null, parameter.Name)
            {
                Reference = parameter,
                AccessKind = node.AccessKind,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        return node;
    }

    public override ISemanticNode TransformMethod(MethodDeclaration node)
        => node;

    public override ISemanticNode TransformParameter(Parameter node)
        => node;

    public override ISemanticNode TransformPointer(PointerType node)
        => node;

    public override ISemanticNode TransformProperty(PropertyDeclaration node)
    {
        var propertyMetadata = node.Metadata!;
        var returnTypeMetadata = propertyMetadata.Type;
        var typeMetadata = (TypeMetadata)propertyMetadata.DeclaringType;

        var hasField = HasField(propertyMetadata.Getter, node.Getter?.Body) ||
                       HasField(propertyMetadata.Setter, node.Setter?.Body);

        if (hasField)
        {
            currentField = new FieldMetadata(typeMetadata, $"<>_{propertyMetadata.Name}", returnTypeMetadata);
            typeMetadata.AddField(currentField);
        }

        var getter = (PropertyGetter?)node.Getter?.Transform(this);
        var setter = (PropertySetter?)node.Setter?.Transform(this);

        currentField = null;

        if (getter == node.Getter && setter == node.Setter)
            return node;

        return new PropertyDeclaration(node.SourceSpan, node.Name, node.Type, getter, setter)
        {
            Metadata = node.Metadata,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
        };
    }

    private static bool HasField(MethodMetadata? accessor, BlockStatement? body)
        => accessor is not null &&
           (body is null || body.Find<MemberAccessExpression>(x => x.IsField) is not null);

    public override ISemanticNode TransformSetter(PropertySetter node)
    {
        currentSetter = node.Metadata!;
        var body = (BlockStatement?)node.Body?.Transform(this);
        currentSetter = null;

        if (body == node.Body)
            return node;

        return new PropertySetter(node.SourceSpan, node.AccessModifier, body)
        {
            Metadata = node.Metadata,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
        };
    }

    public override ISemanticNode TransformTuple(TupleExpression node)
        => node;

    public override ISemanticNode TransformTupleType(TupleType node)
        => node;

    public override ISemanticNode TransformTypeNode(TypeRef node)
        => node;
}