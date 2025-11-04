namespace Trilang.Compilation.Diagnostics;

public readonly record struct DiagnosticId(DiagnosticCategory Category, int Id)
{
    public static readonly DiagnosticId L0001UnsupportedCharacter = ForLexer(1);
    public static readonly DiagnosticId L0002MissingEndQuoteForStringLiteral = ForLexer(2);

    public static readonly DiagnosticId P0001MissingToken = ForParser(1);
    public static readonly DiagnosticId P0002ExpectedParameter = ForParser(2);
    public static readonly DiagnosticId P0003ExpectedType = ForParser(3);
    public static readonly DiagnosticId P0004ExpectedStatement = ForParser(4);
    public static readonly DiagnosticId P0005ExpectedTypeName = ForParser(5);
    public static readonly DiagnosticId P0006ExpectedInterface = ForParser(6);
    public static readonly DiagnosticId P0007ExpectedMethodName = ForParser(7);
    public static readonly DiagnosticId P0008ExpectedVariableName = ForParser(8);
    public static readonly DiagnosticId P0009ExpectedExpression = ForParser(9);
    public static readonly DiagnosticId P0010ExpectedDeclaration = ForParser(10);
    public static readonly DiagnosticId P0011ExpectedDirectiveName = ForParser(11);
    public static readonly DiagnosticId P0012ExpectedInterfaceParameters = ForParser(12);
    public static readonly DiagnosticId P0013ExpectedIdentifier = ForParser(13);
    public static readonly DiagnosticId P0014ExpectedTypeMember = ForParser(14);

    public static readonly DiagnosticId S0001CyclicTypeAlias = ForSemantic(1);
    public static readonly DiagnosticId S0002AlreadyDefined = ForSemantic(2);
    public static readonly DiagnosticId S0003UnknownType = ForSemantic(3);
    public static readonly DiagnosticId S0004ReturnTypeMismatch = ForSemantic(4);
    public static readonly DiagnosticId S0005TypeMismatch = ForSemantic(5);
    public static readonly DiagnosticId S0006ExpectedArray = ForSemantic(6);
    public static readonly DiagnosticId S0007ExpectedFunction = ForSemantic(7);
    public static readonly DiagnosticId S0008UnknownMember = ForSemantic(8);
    public static readonly DiagnosticId S0009CantCreateObject = ForSemantic(9);
    public static readonly DiagnosticId S0010IncompatibleUnaryOperator = ForSemantic(10);
    public static readonly DiagnosticId S0011IncompatibleBinaryOperator = ForSemantic(11);
    public static readonly DiagnosticId S0012BreakOutsideLoop = ForSemantic(12);
    public static readonly DiagnosticId S0013ContinueOutsideLoop = ForSemantic(13);
    public static readonly DiagnosticId S0014MemberNotAccessible = ForSemantic(14);
    public static readonly DiagnosticId S0015NotAllPathsReturnValue = ForSemantic(15);
    public static readonly DiagnosticId S0016ThisInStaticMethod = ForSemantic(16);
    public static readonly DiagnosticId S0017ThisOutsideOfType = ForSemantic(17);
    public static readonly DiagnosticId S0018StaticMethodAsInstance = ForSemantic(18);
    public static readonly DiagnosticId S0019InstanceMethodAsStatic = ForSemantic(19);
    public static readonly DiagnosticId S0020VariableUsedBeforeDeclaration = ForSemantic(20);
    public static readonly DiagnosticId S0021MemberIsNotImplemented = ForSemantic(21);
    public static readonly DiagnosticId S0022InterfacePropertyCantBePrivate = ForSemantic(22);

    private static DiagnosticId ForLexer(int id)
        => new DiagnosticId(DiagnosticCategory.Lexer, id);

    private static DiagnosticId ForParser(int id)
        => new DiagnosticId(DiagnosticCategory.Parser, id);

    private static DiagnosticId ForSemantic(int id)
        => new DiagnosticId(DiagnosticCategory.Semantic, id);

    public override string ToString()
    {
        var category = Category switch
        {
            DiagnosticCategory.Lexer => "L",
            DiagnosticCategory.Parser => "P",
            DiagnosticCategory.Semantic => "S",
            _ => throw new ArgumentOutOfRangeException(nameof(Category)),
        };

        return $"{category}{Id:0000}";
    }
}