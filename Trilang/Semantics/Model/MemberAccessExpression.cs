using System.Diagnostics.CodeAnalysis;
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class MemberAccessExpression : IExpression
{
    public const string This = "this";
    public const string Field = "field";
    public const string Value = "value";

    public MemberAccessExpression(string name) : this(null, name)
    {
    }

    public MemberAccessExpression(IExpression? member, string name)
    {
        Member = member;
        Name = name;

        if (Member is not null)
            Member.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitMemberAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitMemberAccess(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformMemberAccess(this);

    public IExpression Clone()
        => new MemberAccessExpression(Member?.Clone(), Name)
        {
            Reference = Reference,
            AccessKind = AccessKind,
        };

    public ISemanticNode? Parent { get; set; }

    public IExpression? Member { get; set; }

    public string Name { get; }

    public IMetadata? Reference { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Reference switch
        {
            VariableMetadata variable
                => variable.Type,

            ParameterMetadata parameter
                => parameter.Type,

            FieldMetadata field
                => field.Type,

            PropertyMetadata property
                => property.Type,

            MethodMetadata method
                => method.Type,

            FunctionMetadata function
                => function.Type,

            InterfacePropertyMetadata interfaceProperty
                => interfaceProperty.Type,

            InterfaceMethodMetadata interfaceMethod
                => interfaceMethod.Type,

            ITypeMetadata type
                => type,

            null => null,
            _ => throw new InvalidOperationException(),
        };

    public bool IsThis
        => Name == This;

    public bool IsField
        => Name == Field;

    public bool IsValue
        => Name == Value;

    [MemberNotNullWhen(false, nameof(Member))]
    public bool IsFirstMember
        => Member is null;

    public MemberAccessKind? AccessKind { get; set; }
}