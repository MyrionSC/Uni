using System;
using System.Collections.Generic;
using System.Linq;
using assignment3.model;

namespace assignment3
{
    internal class Program
    {
        private static int[,] userMatrix;
        
        public static void Main(string[] args)
        {
            List<Rating> trainingRatings = Parser.ParseRatings("data/u1.base");
            List<Rating> testRatings = Parser.ParseRatings("data/u1.test");

            // create user-item rating matrix
            int usersCount = trainingRatings.Select(r => r.userId).ToArray().Max() + 1;
            int itemsCount = trainingRatings.Select(r => r.itemId).ToArray().Max() + 1;
            userMatrix = new int[usersCount, itemsCount];
            trainingRatings.ForEach(r =>
            {
                userMatrix[r.userId, r.itemId] = r.rating;
            });

            // 1: Determine the similarity of two users
            // 2: Find nearest neighbors to user
            // 3: combine ratings of neighbors to predict ratings

            
            
            
            
            
//            Console.WriteLine(userSimilarity(1, 13));

//            double curmax = 0;
//            int mostSim = 0;
//            for (int i = 1; i < 50; i++)
//            {
//                if (i == 1) continue;
//                
//                var sim = userSimilarity(1, i);
//                Console.WriteLine(i + ": " + sim);
//                if (curmax < sim)
//                {
//                    curmax = sim;
//                    mostSim = i;
//                }
//            }
//            Console.WriteLine();
//            Console.WriteLine(curmax);
//            Console.WriteLine(mostSim);
            
        }

        public static double pred(int user, int item)
        {
//            1. Calculate, whether the neighbors' ratings for the unseen item 𝑝 are higher or lower than their average
//            2. Combine the rating differences – weighted by importance of neighbor
//            3. Add/subtract the neighbors' bias from the active user's average and use this as a prediction
            
            
            return 1;
        }

        public static double weightOfNeighbor(int user, int neighbor);
        {
            //𝒘 (𝒂, 𝒃) = 𝒔𝒊𝒎(𝒂, 𝒃) / sum 𝒃∈𝑵 (𝒔𝒊𝒎(𝒂, 𝒃))
//            var res = userSimilarity(user, neighbor) / userMatrix.
            

            return 1;
        }

        public static double userSimilarity(int userA, int userB) // pearson correlation
        {
            var aItems = userRatedItems(userA);
            var bItems = userRatedItems(userB);
            var intersect = aItems.Intersect(bItems);
            var aAverage = averageRating(userA);
            var bAverage = averageRating(userB);

            double top = 0, bottomLeft = 0, bottomRight = 0;
            foreach (int item in intersect)
            {
                top += (userRatingForItem(userA, item) - aAverage) * (userRatingForItem(userB, item) - bAverage);
                bottomLeft += Math.Pow(userRatingForItem(userA, item), 2);
                bottomRight += Math.Pow(userRatingForItem(userB, item), 2);
            }
            double bottom = Math.Log(bottomLeft) * Math.Log(bottomRight);
            var ret = top / bottom;
            return ret;
        }
        public static double averageRating(int user)
        {
            double ratingSum = 0;
            double ratingCount = 0;
            for (int i = 0; i < userMatrix.GetLength(1); i++)
            {
                if (userMatrix[user, i] != 0)
                {
                    ratingSum += userRatingForItem(user, i);
                    ratingCount++;
                }
            }
            return ratingSum / ratingCount;
        }
        public static HashSet<int> userRatedItems(int user)
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
        public static int userRatingForItem(int user, int item)
        {
            return userMatrix[user, item];
        }
    }
}