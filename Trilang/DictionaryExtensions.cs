namespace Trilang;

internal static class DictionaryExtensions
{
    public static bool DictionaryEquals<TKey, TValue>(
        this Dictionary<TKey, TValue> left,
        Dictionary<TKey, TValue> right)
        where TKey : notnull
    {
        if (left.Count != right.Count)
            return false;

        foreach (var (key, value) in left)
        {
            if (!right.TryGetValue(key, out var otherValue))
                return false;

            if (!Equals(value, otherValue))
                return false;
        }

        return true;
    }
}