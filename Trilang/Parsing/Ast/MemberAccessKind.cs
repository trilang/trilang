namespace Trilang.Parsing.Ast;

[Flags]
public enum MemberAccessKind
{
    Read = 0x1,
    Write = 0x2,
    ReadWrite = Read | Write,
}