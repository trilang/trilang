using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

internal class GenerateMetadata : Visitor
{
    private readonly TypeMetadataProvider typeProvider;

    public GenerateMetadata() : this(new TypeMetadataProvider())
    {
    }

    public GenerateMetadata(TypeMetadataProvider typeProvider)
        => this.typeProvider = typeProvider;

    private void BuildSymbolTableTypes(ISymbolTable? symbolTable)
    {
        if (symbolTable is null)
            throw new ArgumentNullException(nameof(symbolTable));

        CreateTypes(symbolTable.Types);
        CreateInterfaces(symbolTable.Types);
        CreateDiscriminatedUnion(symbolTable.Types);
        CreateAliases(symbolTable.Types);
        CreateTuples(symbolTable.Types);
        CreateArrays(symbolTable.Types);
        BuildFunctionTypes(symbolTable.Types);

        PopulateAliases(symbolTable.Types);
        PopulateInterfaces(symbolTable.Types);
        PopulateTypes(symbolTable.Types);
        PopulateDiscriminatedUnion(symbolTable.Types);
        PopulateTuples(symbolTable.Types);
        BuildFunctions(symbolTable.Ids);
    }

    private void CreateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsType)
                continue;

            var metadata = new TypeMetadata(symbol.Name);

            if (!typeProvider.DefineType(metadata))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    private void PopulateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            var node = symbol.Node;
            if (node is not TypeDeclarationNode typeDeclarationNode)
                continue;

            var metadata = typeProvider.GetType(symbol.Name);
            Debug.Assert(metadata is not null);

            if (metadata is not TypeMetadata type)
                continue;

            foreach (var @interface in typeDeclarationNode.Interfaces)
            {
                var aliasMetadata = typeProvider.GetType(@interface.Name) as TypeAliasMetadata ??
                                    throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not defined.");

                var interfaceMetadata = aliasMetadata.Type as InterfaceMetadata ??
                                        throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not an interface.");

                type.AddInterface(interfaceMetadata);
            }

            foreach (var property in typeDeclarationNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   throw new SemanticAnalysisException($"The '{property.Name}' property has unknown type: '{property.Type.Name}'.");

                var propertyMetadata = new PropertyMetadata(
                    type,
                    GetAccessModifierMetadata(property.AccessModifier),
                    property.Name,
                    propertyType);

                type.AddProperty(propertyMetadata);
            }

            foreach (var constructor in typeDeclarationNode.Constructors)
            {
                var parameters = new ITypeMetadata[constructor.Parameters.Count];
                for (var i = 0; i < parameters.Length; i++)
                {
                    var parameter = constructor.Parameters[i];
                    var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                        throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

                    parameters[i] = parameterType;
                }

                var constructorMetadata = new ConstructorMetadata(
                    type,
                    GetAccessModifierMetadata(constructor.AccessModifier),
                    parameters);

                type.AddConstructor(constructorMetadata);
            }

            foreach (var method in typeDeclarationNode.Methods)
            {
                var parameters = new ITypeMetadata[method.Parameters.Count];
                for (var i = 0; i < parameters.Length; i++)
                {
                    var parameter = method.Parameters[i];
                    var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                        throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

                    parameters[i] = parameterType;
                }

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameters, returnType);
                typeProvider.DefineType(functionType);

                var methodMetadata = new MethodMetadata(
                    type,
                    GetAccessModifierMetadata(method.AccessModifier),
                    method.Name,
                    functionType);

                type.AddMethod(methodMetadata);
            }
        }
    }

    private void CreateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            var metadata = new InterfaceMetadata(symbol.Name);
            if (!typeProvider.DefineType(metadata))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    private void PopulateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            var node = symbol.Node;
            if (node is not InterfaceNode interfaceNode)
                continue;

            if (typeProvider.GetType(symbol.Name) is not InterfaceMetadata metadata)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not an interface.");

            foreach (var property in interfaceNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   throw new SemanticAnalysisException($"The '{property.Name}' property has unknown type: '{property.Type.Name}'.");

                var propertyMetadata = new InterfacePropertyMetadata(metadata, property.Name, propertyType);
                metadata.AddProperty(propertyMetadata);
            }

            foreach (var method in interfaceNode.Methods)
            {
                var parameters = new ITypeMetadata[method.Parameters.Count];
                for (var i = 0; i < parameters.Length; i++)
                {
                    var parameter = method.Parameters[i];
                    var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                        throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

                    parameters[i] = parameterType;
                }

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameters, returnType);
                typeProvider.DefineType(functionType);

                var methodMetadata = new InterfaceMethodMetadata(metadata, method.Name, functionType);
                metadata.AddMethod(methodMetadata);
            }
        }
    }

    private void CreateDiscriminatedUnion(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            var metadata = new DiscriminatedUnionType(symbol.Name);
            typeProvider.DefineType(metadata);
        }
    }

    private void PopulateDiscriminatedUnion(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            var node = symbol.Node;
            if (node is not DiscriminatedUnionNode discriminatedUnionNode)
                continue;

            if (typeProvider.GetType(symbol.Name) is not DiscriminatedUnionType metadata)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not a discriminated union.");

            foreach (var typeNode in discriminatedUnionNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                metadata.AddType(type);
            }
        }
    }

    private void CreateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (symbol is not { IsAlias: true, Node: TypeAliasDeclarationNode })
                continue;

            var alias = new TypeAliasMetadata(symbol.Name);
            if (!typeProvider.DefineType(alias))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    private void PopulateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        // all array and alias types are processed after all types are defined to support forward references
        foreach (var (_, symbol) in types)
        {
            if (symbol is not { IsAlias: true, Node: TypeAliasDeclarationNode typeAliasNode })
                continue;

            var type = typeProvider.GetType(typeAliasNode.Name);
            if (type is not TypeAliasMetadata aliasMetadata)
                throw new SemanticAnalysisException($"The '{typeAliasNode.Name}' type is not an alias.");

            var aliasedMetadata = typeProvider.GetType(typeAliasNode.Type.Name) ??
                                  throw new SemanticAnalysisException($"The '{typeAliasNode.Type.Name}' aliased type is not defined.");

            aliasMetadata.Type = aliasedMetadata;
        }
    }

    private void CreateArrays(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsArray)
                continue;

            if (symbol.Node is not ArrayTypeNode arrayTypeNode)
                throw new SemanticAnalysisException();

            var type = typeProvider.GetType(arrayTypeNode.ElementType.Name) ??
                       throw new SemanticAnalysisException($"The '{arrayTypeNode.ElementType.Name}' array item type is not defined.");

            var metadata = new TypeArrayMetadata(type);
            if (typeProvider.GetType(metadata.Name) is null)
                typeProvider.DefineType(metadata);
        }
    }

    private void CreateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            var tuple = new TupleMetadata(symbol.Name);
            typeProvider.DefineType(tuple);
        }
    }

    private void PopulateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            if (symbol.Node is not TupleTypeNode tupleNode)
                throw new SemanticAnalysisException();

            if (typeProvider.GetType(symbol.Name) is not TupleMetadata tuple)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not a tuple.");

            if (tuple.Types.Count > 0)
                continue;

            foreach (var typeNode in tupleNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                tuple.AddType(type);
            }
        }
    }

    private void BuildFunctionTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsFunction)
                continue;

            if (symbol.Node is not FunctionTypeNode function)
                throw new SemanticAnalysisException($"The '{symbol.Name}' symbol is not a function.");

            var parameterTypes = new ITypeMetadata[function.ParameterTypes.Count];
            for (var i = 0; i < parameterTypes.Length; i++)
            {
                var parameterType = function.ParameterTypes[i];
                var type = typeProvider.GetType(parameterType.Name) ??
                           throw new SemanticAnalysisException($"The function has unknown parameter type: '{parameterType.Name}'.");

                parameterTypes[i] = type;
            }

            var returnType = typeProvider.GetType(function.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{function.ReturnType.Name}'.");

            var functionTypeMetadata = new FunctionTypeMetadata(parameterTypes, returnType);
            if (typeProvider.GetType(functionTypeMetadata.Name) is null)
                typeProvider.DefineType(functionTypeMetadata);
        }
    }

    private void BuildFunctions(IReadOnlyDictionary<string, IdSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            if (symbol.Node is not FunctionDeclarationNode function)
                continue;

            if (function.Metadata is not null)
                continue;

            var parameters = new ITypeMetadata[function.Parameters.Count];
            for (var i = 0; i < function.Parameters.Count; i++)
            {
                var parameter = function.Parameters[i];
                var type = typeProvider.GetType(parameter.Type.Name);

                parameters[i] = type ??
                                throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");
            }

            var returnType = typeProvider.GetType(function.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{function.ReturnType.Name}'.");

            var functionTypeMetadata = new FunctionTypeMetadata(parameters, returnType);
            typeProvider.DefineType(functionTypeMetadata);
        }
    }

    private static AccessModifierMetadata GetAccessModifierMetadata(AccessModifier accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Private => AccessModifierMetadata.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };

    protected override void VisitEnter(SyntaxTree node)
        => BuildSymbolTableTypes(node.SymbolTable);
}