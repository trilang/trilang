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
            foreach (var interfaceField in @interface.Fields)
            {
                var typeField = type.GetField(interfaceField.Name) ??
                                throw new SemanticAnalysisException($"The '{interfaceField.Name}' field is not implemented.");

                if (!interfaceField.Type.Equals(typeField.Type))
                    throw new SemanticAnalysisException($"The '{interfaceField.Name}' field is not of the correct type.");
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