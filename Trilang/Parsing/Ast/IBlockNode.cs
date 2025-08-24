namespace Trilang.Parsing.Ast;

public interface IBlockNode
{
    void Add(IStatementNode declaration);

    void Insert(int i, IStatementNode declaration);

    void InsertAfter(IStatementNode declaration, IStatementNode after);

    void Replace(IStatementNode oldStatement, IStatementNode newStatement);

    void Remove(IStatementNode declaration);
}