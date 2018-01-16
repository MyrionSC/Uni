namespace wi_assignment_2
{
    public class Review
    {
        public int Score;
        public string Summary;
        public string Text;

        public Review()
        {
            Score = 0;
            Summary = "";
            Text = "";
        }
        public Review(int Score, string summary, string text)
        {
            this.Score = Score;
            Summary = summary;
            Text = text;
        }
    }
}