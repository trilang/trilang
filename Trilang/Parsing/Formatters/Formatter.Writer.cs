using System.Text;

namespace Trilang.Parsing.Formatters;

public partial class Formatter
{
    private sealed class Writer
    {
        private readonly StringBuilder sb;
        private Indent indent;
        private bool isIndented;

        private static ReadOnlySpan<char> Indentation => [' ', ' ', ' ', ' '];
        private const string NewLine = "\n";

        public Writer()
        {
            sb = new StringBuilder();
            indent = new Indent();
            isIndented = false;
        }

        private void Increase()
            => indent = indent.Increase();

        private void Decrease()
            => indent = indent.Decrease();

        private void WriteIndent()
        {
            if (isIndented)
                return;

            for (var i = 0; i < indent.Length; i++)
                sb.Append(Indentation);

            isIndented = true;
        }

        public void Scoped(Action action)
        {
            Increase();

            action();

            Decrease();
        }

        public void Write(char c)
        {
            WriteIndent();
            sb.Append(c);
        }

        public void Write(string str)
        {
            WriteIndent();
            sb.Append(str);
        }

        public void WriteLine()
            => WriteLine(string.Empty);

        public void WriteLine(char c)
        {
            WriteIndent();
            sb.Append($"{c}{NewLine}");

            isIndented = false;
        }

        public void WriteLine(string str)
        {
            WriteIndent();
            sb.Append($"{str}{NewLine}");

            isIndented = false;
        }


        public void RemoveLastNewLine()
        {
            var newLine = Environment.NewLine;
            var newLineLength = newLine.Length;
            if (sb.Length < newLineLength)
                return;

            var lastChars = sb.ToString(sb.Length - newLineLength, newLineLength);
            if (lastChars == newLine)
            {
                sb.Length -= newLineLength;
                CancelIndent();
            }
        }

        public void CancelIndent()
            => isIndented = true;

        public override string ToString()
            => sb.ToString();
    }

    private readonly struct Indent
    {
        public Indent()
            : this(0)
        {
        }

        private Indent(int level)
        {
            if (level < 0)
                throw new ArgumentOutOfRangeException(nameof(level));

            Length = level;
        }

        public Indent Increase()
            => new Indent(Length + 1);

        public Indent Decrease()
            => new Indent(Length - 1);

        public int Length { get; }
    }
}