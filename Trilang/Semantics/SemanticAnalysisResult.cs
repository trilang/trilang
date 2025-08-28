using Trilang.Metadata;
using Trilang.Semantics.Passes;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(SymbolTableMap SymbolTableMap, ITypeMetadataProvider TypeMetadataProvider);