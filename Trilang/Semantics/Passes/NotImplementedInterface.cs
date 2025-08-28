using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class NotImplementedInterface : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitTypeEnter(TypeDeclarationNode node)
    {
        var type = node.Metadata;
        if (node.Interfaces.Count == 0 || type is null)
            return;

        foreach (var @interface in type.Interfaces)
        {
            foreach (var interfaceProperty in @interface.Properties)
            {
                var propertyType = type.GetProperty(interfaceProperty.Name) ??
                                   throw new SemanticAnalysisException($"The '{interfaceProperty.Name}' property is not implemented.");

                if (!interfaceProperty.Type.Equals(propertyType.Type))
                    throw new SemanticAnalysisException($"The '{interfaceProperty.Name}' property is not of the correct type.");

                if (interfaceProperty.GetterModifier == AccessModifierMetadata.Public &&
                    propertyType.Getter?.AccessModifier != AccessModifierMetadata.Public)
                    throw new SemanticAnalysisException($"The implementation of an interface property getter '{interfaceProperty.Name}' cannot be private.");

                if (interfaceProperty.SetterModifier == AccessModifierMetadata.Public &&
                    propertyType.Setter?.AccessModifier != AccessModifierMetadata.Public)
                    throw new SemanticAnalysisException($"The implementation of an interface property setter '{interfaceProperty.Name}' cannot be private.");
            }

            foreach (var interfaceMethod in @interface.Methods)
            {
                var method = type.GetMethod(interfaceMethod.Name) ??
                             throw new SemanticAnalysisException($"The '{interfaceMethod.Name}' method is not implemented.");

                if (!interfaceMethod.Type.Equals(method.Type))
                    throw new SemanticAnalysisException($"The '{interfaceMethod.Name}' method is not of the correct type.");

                if (method.AccessModifier == AccessModifierMetadata.Private)
                    throw new SemanticAnalysisException($"The implementation of an interface method '{interfaceMethod.Name}' cannot be private.");
            }
        }
    }

    public string Name => nameof(NotImplementedInterface);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}