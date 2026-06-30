using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;

namespace Trilang.Lower;

internal class ReplaceCompoundAssignments : Transformer
{
    public ReplaceCompoundAssignments(ISet<string> directives, BuiltInTypes builtInTypes)
        : base(directives, builtInTypes)
    {
    }

    public override ISemanticNode TransformAlias(AliasDeclaration node)
        => node;

    public override ISemanticNode TransformArrayType(ArrayType node)
        => node;

    public override ISemanticNode TransformBinaryExpression(BinaryExpression node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);

        if (node.Kind.IsCompoundAssignment())
        {
            var kind = node.Kind switch
            {
                AdditionAssignment => Addition,
                SubtractionAssignment => Subtraction,
                MultiplicationAssignment => Multiplication,
                DivisionAssignment => Division,
                ModulusAssignment => Modulus,
                BitwiseAndAssignment => BitwiseAnd,
                BitwiseOrAssignment => BitwiseOr,
                BitwiseXorAssignment => BitwiseXor,
                _ => throw new ArgumentOutOfRangeException(),
            };

            var read = (MemberAccessExpression)left.Clone();
            read.AccessKind = MemberAccessKind.Read;

            var write = (MemberAccessExpression)left.Clone();
            write.AccessKind = MemberAccessKind.Write;

            right = new BinaryExpression(null, kind, read, right)
            {
                ReturnTypeMetadata = node.ReturnTypeMetadata,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };

            return new BinaryExpression(null, Assignment, write, right)
            {
                ReturnTypeMetadata = node.ReturnTypeMetadata,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        if (ReferenceEquals(left, node.Left) && ReferenceEquals(right, node.Right))
            return node;

        return new BinaryExpression(null, node.Kind, left, right)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
        };
    }

    public override ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnion node)
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

    public override ISemanticNode TransformParameter(Parameter node)
        => node;

    public override ISemanticNode TransformPointer(PointerType node)
        => node;

    public override ISemanticNode TransformTupleType(TupleType node)
        => node;

    public override ISemanticNode TransformTypeNode(TypeRef node)
        => node;
}