using Trilang.Metadata;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(SymbolTableMap SymbolTableMap, ITypeMetadataProvider TypeMetadataProvider);