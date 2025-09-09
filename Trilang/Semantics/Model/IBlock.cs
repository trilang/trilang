namespace Trilang.Semantics.Model;

public interface IBlock
{
    void Add(IStatement declaration);

    void Insert(int i, IStatement declaration);

    void InsertAfter(IStatement declaration, IStatement after);

    void Replace(IStatement oldStatement, IStatement newStatement);

    void Remove(IStatement declaration);
}