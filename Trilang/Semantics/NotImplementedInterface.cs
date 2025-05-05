using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

public class NotImplementedInterface : Visitor
{
    protected override void VisitEnter(TypeDeclarationNode node)
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
            }

            foreach (var interfaceMethod in @interface.Methods)
            {
                var typeMethod = type.GetMethod(interfaceMethod.Name) ??
                                 throw new SemanticAnalysisException($"The '{interfaceMethod.Name}' method is not implemented.");

                if (!interfaceMethod.TypeMetadata.Equals(typeMethod.TypeMetadata))
                    throw new SemanticAnalysisException($"The '{interfaceMethod.Name}' method is not of the correct type.");
            }
        }
    }
}