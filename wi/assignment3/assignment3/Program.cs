using System;
using System.Collections.Generic;
using System.Linq;
using assignment3.model;

namespace assignment3
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            List<Rating> trainingRatings = Parser.ParseRatings("data/u1.base");
            List<Rating> testRatings = Parser.ParseRatings("data/u1.test");

            // create user-item rating matrix
            int usersCount = trainingRatings.Select(r => r.userId).ToArray().Max() + 1;
            int itemsCount = trainingRatings.Select(r => r.itemId).ToArray().Max() + 1;
            int[,] userMatrix = new int[usersCount, itemsCount];
            trainingRatings.ForEach(r =>
            {
                userMatrix[r.userId, r.itemId] = r.rating;
            });

            // 1: Determine the similarity of two users
            // 2: Find nearest neighbors to user
            // 3: combine ratings of neighbors to predict ratings

            userSimilarity(2, 3, userMatrix);

            Console.WriteLine(averageRating(3, userMatrix));

        }

        public static double userSimilarity(int userA, int userB, int[,] userMatrix)
        {
            var aItems = userRatedItems(userA, userMatrix);
            var bItems = userRatedItems(userB, userMatrix);
            var Union = aItems.Union(bItems);
            var Intersect = aItems.Intersect(bItems);

            return userA;
        }
        
        
        
        public static double averageRating(int user, int[,] userMatrix)
        {
            double ratingSum = 0;
            double ratingCount = 0;
            for (int i = 0; i < userMatrix.GetLength(1); i++)
            {
                if (userMatrix[user, i] != 0)
                {
                    ratingSum += userRatingForItem(user, i, userMatrix);
                    ratingCount++;
                }
            }
            return ratingSum / ratingCount;
        }

        public static HashSet<int> userRatedItems(int user, int[,] userMatrix)
        {
            HashSet<int> items = new HashSet<int>();
            for (int i = 0; i < userMatrix.GetLength(1); i++)
            {
                if (userMatrix[user, i] != 0)
                {
                    items.Add(i);
                }
            }
            return items;
        }
        
        public static int userRatingForItem(int user, int item, int[,] userMatrix)
        {
            return userMatrix[user, item];
        }
    }
}