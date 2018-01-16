using System.Linq;
using System.Text;

namespace wi_assignment_2
{
    public class Tokenizer
    {
        public static string[] Tokenize(string input)
        {
            string NoSymbolsInput = RemoveSymbols(input);
            string[] tokensWithTrash = NoSymbolsInput.Split(' ');
            string[] tokens = tokensWithTrash.Where(item => item != "" && item != " ").ToArray();
            string[] tokensLower = new string[tokens.Length];
            for (var j = 0; j < tokens.Length; j++)
            {
                tokensLower[j] = tokens[j].ToLower();
            }

            return tokensLower;
        }
        
        private static string RemoveSymbols(string input)
        {
            var sb = new StringBuilder();
            foreach (char c in input)
            {
                if (char.IsLetterOrDigit(c))
                    sb.Append(c);
                else
                    sb.Append(' ');
            }
            return sb.ToString();
        }
    }
}