namespace wi_assignment_2
{
    public class Review
    {
        public bool Positive;
        public string Summary;
        public string Text;

        public Review()
        {
            Positive = false;
            Summary = "";
            Text = "";
        }
        public Review(int score, string summary, string text)
        {
            Positive = score > 2;
            Summary = summary;
            Text = text;
        }
    }
}