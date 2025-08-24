using System.Diagnostics;
using Trilang.IntermediateRepresentation.Instructions;
using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

internal class SsaRenamer
{
    private readonly Block entry;
    private readonly Dictionary<Block, List<Block>> dominatorsTree;
    private readonly Dictionary<Register, Stack<Register>> registers;
    private int registerCounter;

    public SsaRenamer(int registerCounter, IReadOnlyList<Block> blocks, Dictionary<Block, Block> dominators)
    {
        entry = blocks[0];
        dominatorsTree = GetDominatorTree(dominators);
        registers = [];
        this.registerCounter = registerCounter;
    }

    private Dictionary<Block, List<Block>> GetDominatorTree(Dictionary<Block, Block> dominators)
    {
        var tree = new Dictionary<Block, List<Block>>();

        foreach (var (block, _) in dominators)
            tree[block] = [];

        foreach (var (block, idom) in dominators)
        {
            if (block == idom)
                continue;

            tree[idom].Add(block);
        }

        return tree;
    }

    public void Rename()
        => RenameVariables(entry);

    private void RenameVariables(Block block)
    {
        for (var i = 0; i < block.Instructions.Count; i++)
        {
            var instruction = block.Instructions[i];

            if (instruction is Alloc)
            {
                // nothing to rename here
            }
            else if (instruction is ArrayAlloc arrayAlloc)
            {
                block.ReplaceInstruction(
                    i,
                    arrayAlloc with { Count = Rename(arrayAlloc.Count) });
            }
            else if (instruction is GetElementPointer arrayElement)
            {
                block.ReplaceInstruction(
                    i,
                    arrayElement with
                    {
                        Array = Rename(arrayElement.Array),
                        Index = Rename(arrayElement.Index),
                    });
            }
            else if (instruction is BinaryOperation binary)
            {
                block.ReplaceInstruction(
                    i,
                    binary with
                    {
                        Left = Rename(binary.Left),
                        Right = Rename(binary.Right),
                    });
            }
            else if (instruction is Branch branch)
            {
                block.ReplaceInstruction(
                    i,
                    branch with { Condition = Rename(branch.Condition) });
            }
            else if (instruction is Call call)
            {
                call = call with
                {
                    Parameters = call.Parameters.Select(Rename).ToArray(),
                    Function = Rename(call.Function),
                };
                block.ReplaceInstruction(i, call);

                if (!registers.TryGetValue(call.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(call.Result);

                    registers[call.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(call.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, call with { Result = newRegister });
                }
            }
            else if (instruction is Cast cast)
            {
                cast = cast with { Source = Rename(cast.Source) };
                block.ReplaceInstruction(i, cast);

                if (!registers.TryGetValue(cast.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(cast.Result);

                    registers[cast.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(cast.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, cast with { Result = newRegister });
                }
            }
            else if (instruction is IsType isType)
            {
                isType = isType with { Source = Rename(isType.Source) };
                block.ReplaceInstruction(i, isType);

                if (!registers.TryGetValue(isType.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(isType.Result);

                    registers[isType.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(isType.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, isType with { Result = newRegister });
                }
            }
            else if (instruction is Jump)
            {
                // nothing to rename here
            }
            else if (instruction is GetMemberPointer getMember)
            {
                if (getMember.Source is not null)
                {
                    getMember = getMember with { Source = Rename(getMember.Source.Value) };
                    block.ReplaceInstruction(i, getMember);
                }

                if (!registers.TryGetValue(getMember.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(getMember.Result);

                    registers[getMember.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(getMember.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, getMember with { Result = newRegister });
                }
            }
            else if (instruction is Load load)
            {
                load = load with { Source = Rename(load.Source) };
                block.ReplaceInstruction(i, load);
            }
            else if (instruction is LoadConst)
            {
                // no need to rename registers here
                // load instruction is mainly used to load a const to a new register
            }
            else if (instruction is LoadParameter loadParameter)
            {
                if (!registers.TryGetValue(loadParameter.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(loadParameter.Result);

                    registers[loadParameter.Result] = stack;
                }
                else
                {
                    Debug.Fail("Imposibru!");
                }
            }
            else if (instruction is Move move)
            {
                move = move with { Source = Rename(move.Source) };
                block.ReplaceInstruction(i, move);

                if (!registers.TryGetValue(move.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(move.Result);

                    registers[move.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(move.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, move with { Result = newRegister });
                }
            }
            else if (instruction is Phi phi)
            {
                if (!registers.TryGetValue(phi.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(phi.Result);

                    registers[phi.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister(phi.Result.Type);
                    stack.Push(newRegister);
                    block.ReplaceInstruction(
                        i,
                        phi with { Result = newRegister });
                }
            }
            else if (instruction is Return ret)
            {
                if (ret.Expression is null)
                    continue;

                block.ReplaceInstruction(i, new Return(Rename(ret.Expression.Value)));
            }
            else if (instruction is Store store)
            {
                store = new Store(Rename(store.Destination), Rename(store.Source));
                block.ReplaceInstruction(i, store);
            }
            else if (instruction is UnaryOperation unary)
            {
                block.ReplaceInstruction(
                    i,
                    unary with { Operand = Rename(unary.Operand) });
            }
            else
            {
                throw new Exception();
            }
        }

        foreach (var next in block.Next)
        {
            for (var i = 0; i < next.Instructions.Count; i++)
            {
                var instruction = next.Instructions[i];
                if (instruction is not Phi phi)
                    break;

                if (!registers.TryGetValue(phi.Result, out var stack))
                    stack = registers.FirstOrDefault(x => x.Value.Contains(phi.Result)).Value;

                if (stack is null || stack.Count == 0)
                    throw new Exception();

                next.ReplaceInstruction(
                    i,
                    phi with { Sources = [..phi.Sources, stack.Peek()] });
            }
        }

        foreach (var child in dominatorsTree[block])
            RenameVariables(child);

        foreach (var (_, (register, isDefinition)) in block.Assignments)
        {
            if (!isDefinition)
                continue;

            if (!registers.TryGetValue(register, out var stack))
                continue;

            stack.Pop();
        }
    }

    private Register GetNewRegister(ITypeMetadata type)
        => new Register(registerCounter++, type);

    private Register Rename(Register register)
        => registers.TryGetValue(register, out var stack)
            ? stack.Peek()
            : register;
}