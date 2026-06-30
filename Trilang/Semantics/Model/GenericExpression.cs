using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class GenericExpression : IAccessExpression
{
    public GenericExpression(
        SourceSpan? sourceSpan,
        MemberAccessExpression member,
        IReadOnlyList<TypeRef> genericArguments)
    {
        SourceSpan = sourceSpan;
        Member = member;
        GenericArguments = genericArguments;

        Member.Parent = this;
        foreach (var genericArgument in genericArguments)
            genericArgument.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGenericExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGenericExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformGenericExpression(this);

    public IExpression Clone()
        => new GenericExpression(
            SourceSpan,
            (MemberAccessExpression)Member.Clone(),
            GenericArguments.Select(t => (TypeRef)t.Clone()).ToArray())
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public ITypeMetadata ReturnTypeMetadata
        => Member.ReturnTypeMetadata;

    public MemberAccessExpression Member { get; }

    public IReadOnlyList<TypeRef> GenericArguments { get; }

    public IMetadata? Reference
    {
        get => Member.Reference;
        set => Member.Reference = value;
    }
}