using System.Diagnostics;
using Trilang.IntermediateRepresentation.Instructions;

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

            if (instruction is ArrayElement arrayElement)
            {
                block.ReplaceInstruction(
                    i,
                    arrayElement with
                    {
                        Array = Rename(arrayElement.Array),
                        Index = Rename(arrayElement.Index),
                    });
            }
            else if (instruction is BinaryOperation binaryInstruction)
            {
                block.ReplaceInstruction(
                    i,
                    binaryInstruction with
                    {
                        Left = Rename(binaryInstruction.Left),
                        Right = Rename(binaryInstruction.Right),
                    });
            }
            else if (instruction is Branch branchInstruction)
            {
                block.ReplaceInstruction(
                    i,
                    branchInstruction with { Condition = Rename(branchInstruction.Condition) });
            }
            else if (instruction is Jump)
            {
                // nothing to rename here
            }
            else if (instruction is LoadConst)
            {
                // no need to rename registers here
                // load instruction is mainly used to load a const to a new register
            }
            else if (instruction is LoadParameter loadParameterInstruction)
            {
                if (!registers.TryGetValue(loadParameterInstruction.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(loadParameterInstruction.Result);

                    registers[loadParameterInstruction.Result] = stack;
                }
                else
                {
                    Debug.Fail("Imposibru!");
                }
            }
            else if (instruction is Move moveInstruction)
            {
                moveInstruction = moveInstruction with { Source = Rename(moveInstruction.Source) };
                block.ReplaceInstruction(i, moveInstruction);

                if (!registers.TryGetValue(moveInstruction.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(moveInstruction.Result);

                    registers[moveInstruction.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister();
                    stack.Push(newRegister);
                    block.ReplaceInstruction(i, moveInstruction with { Result = newRegister });
                }
            }
            else if (instruction is NewArray newArrayInstruction)
            {
                block.ReplaceInstruction(
                    i,
                    newArrayInstruction with { Size = Rename(newArrayInstruction.Size) });
            }
            else if (instruction is NewObject newObjectInstruction)
            {
                block.ReplaceInstruction(
                    i,
                    newObjectInstruction with
                    {
                        Arguments = newObjectInstruction.Arguments.Select(Rename).ToArray()
                    });
            }
            else if (instruction is Phi phiInstruction)
            {
                if (!registers.TryGetValue(phiInstruction.Result, out var stack))
                {
                    stack = new Stack<Register>();
                    stack.Push(phiInstruction.Result);

                    registers[phiInstruction.Result] = stack;
                }
                else
                {
                    var newRegister = GetNewRegister();
                    stack.Push(newRegister);
                    block.ReplaceInstruction(
                        i,
                        new Phi(newRegister, phiInstruction.Sources));
                }
            }
            else if (instruction is Return returnInstruction)
            {
                if (returnInstruction.Expression is null)
                    continue;

                block.ReplaceInstruction(
                    i,
                    new Return(Rename(returnInstruction.Expression.Value)));
            }
            else if (instruction is UnaryOperation unaryInstruction)
            {
                block.ReplaceInstruction(
                    i,
                    unaryInstruction with { Operand = Rename(unaryInstruction.Operand) });
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
                    new Phi(phi.Result, [..phi.Sources, stack.Peek()]));
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

    private Register GetNewRegister()
        => new Register(registerCounter++);

    private Register Rename(Register register)
        => registers.TryGetValue(register, out var stack)
            ? stack.Peek()
            : register;
}