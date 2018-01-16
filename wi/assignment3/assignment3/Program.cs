using System;
using System.Collections.Generic;
using System.Linq;
using assignment3.model;
using Microsoft.Win32;

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
            trainingRatings.ForEach(r => {
                userMatrix[r.userId, r.itemId] = r.rating;
            });

            // 1: Determine the similarity of two users
            // 2: Find nearest neighbors to user
            // 3: combine ratings of neighbors to predict ratings

            HashSet<int> nearestNeighbors = findNearestNeigbors(1, usersCount);
            Console.WriteLine(PredictRank(1, 33, nearestNeighbors));
            
            double curmax = 0;
            int highestRank = 0;
            for (int i = 1; i < itemsCount; i++)
            {
                double rankPredicted = PredictRank(1, i, nearestNeighbors);
                Console.WriteLine(i + ": " + rankPredicted);
                if (curmax < rankPredicted)
                {
                    curmax = rankPredicted;
                    highestRank = i;
                }
            }
            Console.WriteLine();
            Console.WriteLine(curmax);
            Console.WriteLine(highestRank);
        }

        public static HashSet<int> findNearestNeigbors(int user, int usersCount)
        {
            HashSet<int> neighbors = new HashSet<int>();
            for (int i = 1; i < usersCount; i++)
            {
                if (i == user) continue;
                
                var sim = userSimilarity(1, i);
                if (sim > 0.7)
                {
                    neighbors.Add(i);
                }
            }
            return neighbors;
        }

        public static double PredictRank(int user, int item, HashSet<int> neighbors)
        {
//            1. Calculate, whether the neighbors' ratings for the unseen item 𝑝 are higher or lower than their average
//            2. Combine the rating differences – weighted by importance of neighbor
//            3. Add/subtract the neighbors' bias from the active user's average and use this as a prediction
            double neighborSimilaritySum = 0;
            foreach (int neighbor in neighbors)
            {
                neighborSimilaritySum += userSimilarity(user, neighbor);
            }
            

            double userAverage = averageRating(user);
            double sumWeightedNeighborRatings = 0;
            foreach (int neighbor in neighbors)
            {
                sumWeightedNeighborRatings += weightOfNeighbor(user, neighbor, neighbors, neighborSimilaritySum) *
                                              (userRatingForItem(neighbor, item) - averageRating(neighbor));
            }
            
            return userAverage + sumWeightedNeighborRatings;
        }

        public static double weightOfNeighbor(int userA, int userB, HashSet<int> neighbors, double neighborSimilaritySum)
        {
            //𝒘 (𝒂, 𝒃) = 𝒔𝒊𝒎(𝒂, 𝒃) / sum 𝒃∈𝑵 (𝒔𝒊𝒎(𝒂, 𝒃))
            return userSimilarity(userA, userB) / neighborSimilaritySum;
        }

        public static double userSimilarity(int userA, int userB) // pearson correlation
        {
            var aItems = userRatedItems(userA);
            var bItems = userRatedItems(userB);
            var intersect = new HashSet<int>(aItems.Intersect(bItems));
            var aAverage = averageRating(userA);
            var bAverage = averageRating(userB);

            double top = 0, bottomLeft = 0, bottomRight = 0;
            foreach (int item in intersect)
            {
                var topLeft = userRatingForItem(userA, item) - aAverage;
                var topRight = userRatingForItem(userB, item) - bAverage;
                top += topLeft * topRight;
                bottomLeft += Math.Pow(userRatingForItem(userA, item) - aAverage, 2);
                bottomRight += Math.Pow(userRatingForItem(userB, item) - bAverage, 2);
            }
            double bottom = Math.Sqrt(bottomLeft) * Math.Sqrt(bottomRight);
            
            if (top == 0 && bottom == 0) return 0; // if no items in common
            return top / bottom;
        }
        public static double averageRating(int user)
        {
            double ratingSum = 0;
            double itemCount = 0;
            
            foreach (int item in userRatedItems(user))
            {
                ratingSum += userRatingForItem(user, item);
                itemCount++;
            }
            return ratingSum / itemCount;
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