using System.Diagnostics.CodeAnalysis;
using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class MemberAccessExpressionNode : IExpressionNode, IEquatable<MemberAccessExpressionNode>
{
    public const string This = "this";
    public const string Field = "field";
    public const string Value = "value";

    public MemberAccessExpressionNode(string name) : this(null, name)
    {
    }

    public MemberAccessExpressionNode(IExpressionNode? member, string name)
    {
        Member = member;
        Name = name;

        if (Member is not null)
            Member.Parent = this;
    }

    public static bool operator ==(MemberAccessExpressionNode? left, MemberAccessExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(MemberAccessExpressionNode? left, MemberAccessExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(MemberAccessExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Equals(Member, other.Member) &&
               Equals(Reference, other.Reference);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((MemberAccessExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Member, Name);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitMemberAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitMemberAccess(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformMemberAccess(this);

    public IExpressionNode Clone()
        => new MemberAccessExpressionNode(Member?.Clone(), Name)
        {
            Reference = Reference,
            AccessKind = AccessKind,
        };

    public ISyntaxNode? Parent { get; set; }

    public IExpressionNode? Member { get; set; }

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