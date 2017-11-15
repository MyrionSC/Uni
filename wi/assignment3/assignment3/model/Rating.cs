namespace assignment3.model
{
    public class Rating
    {
        public int userId;
        public int itemId;
        public int rating;

        public Rating(int userId, int itemId, int rating)
        {
            this.userId = userId;
            this.itemId = itemId;
            this.rating = rating;
        }
    }
}