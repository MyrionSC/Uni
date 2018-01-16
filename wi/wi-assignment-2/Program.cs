using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using MathNet.Numerics.LinearAlgebra.Double;
using MathNet.Numerics.LinearAlgebra.Factorization;


namespace wi_assignment_2
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            DateTime startTime = DateTime.Now;

//            List<User> users = Parser.ParseUsers("friendships.reviews.txt");
//            // With the current spectral cut algorithm, it only makes sense to cut twice. The rest of the cuts only a few people are shaven of a community each cut
//            List<List<User>> communities = Parser.ParseCommunitites(users, 2);
//
//            Utils.PrintCommunities(communities);

            
            
            
            List<Review> reviews = Parser.ParseReviews("SentimentTestingData.txt");
            Dictionary<string, int> vocabulary = new Dictionary<string, int>();
            Dictionary<string, int> vocabularyPositive = new Dictionary<string, int>();

            reviews.ForEach(r =>
            {
                string[] tokens = Tokenizer.Tokenize(r.Text);
                foreach (var t in tokens)
                {
                    if (vocabulary.ContainsKey(t))
                    {
                        vocabulary[t]++;
                        if (r.Positive)
                            vocabularyPositive[t]++;
                    }
                    else
                    {
                        vocabulary.Add(t, 1);
                        vocabularyPositive.Add(t, 0);
                        if (r.Positive)
                            vocabularyPositive[t]++;
                    }   
                }
            });
            
            Console.WriteLine("Review count: " + reviews.Count);
            Console.WriteLine("vocabulary count: " + vocabulary.Count);
            Console.WriteLine();
            
            string test1 =
                "Best granola cereal I've ever eaten!  You don't have to soak it before eating, lol."; // score: 5
            string test2 =
                "I ordered this product thinking it was going to come in a box of 24 (as it is described in the product description). " +
                "I was a little unsure of whether or not this could be too good to be true, but when I saw that the weight of the item was " +
                "marked as 6.1 lbs and the shipping/handling was $8 I felt pretty confident that I would be receiving the 24 count box that I " +
                "thought this to be. When I received the package there was only ONE bone in it. So pretty much I spent $10 on ONE bone that I " +
                "could have gotten at the store for $3. Dont make the same mistake as me! VERY poor product description by Ozbo!"; // score: 1
            string test3 =
                "If you don't have the time to cut up ginger and boil it for it's wonderful flavor here's an alternative.  This tea is refreshing.  " +
                "True ginger flavor and punch is wonderful."; // score: 4
            string test4 =
                "I had a good experience with other Amazon grocery products and these were a good deal, so I bought a three pack. They weren't horrible " +
                "but they don't taste the same as the ones you buy in the store. They taste like cheap generic chocolate chip cookies. Which is not " +
                "necessarily bad, but not what I expected for the price and Chips Ahoy brand. They are cheaper tasting, a little stale and much " +
                "less chocolate chips. I haven't had store-bought Chips Ahoy for a few months so maybe they changed the formula for all these cookies, " +
                "but if not, then these seem to be cheap knock-offs made with cheaper ingredients."; // score: 1

            Console.WriteLine(contentPositiveProbability(Tokenizer.Tokenize(test1), reviews, vocabulary, vocabularyPositive));
            Console.WriteLine(contentPositiveProbability(Tokenizer.Tokenize(test2), reviews, vocabulary, vocabularyPositive));
            Console.WriteLine(contentPositiveProbability(Tokenizer.Tokenize(test3), reviews, vocabulary, vocabularyPositive));
            Console.WriteLine(contentPositiveProbability(Tokenizer.Tokenize(test4), reviews, vocabulary, vocabularyPositive));

            Console.WriteLine();
            Console.WriteLine("-------------------------------");
            Console.WriteLine("Execution time: " + (DateTime.Now - startTime));
        }


        static public double contentPositiveProbability(string[] content, List<Review> reviews,
            Dictionary<string, int> vocabulary, Dictionary<string, int> vocabularyPositive)
        {
            // N = number of reviews
            // P(Review|C) = P(word1|C)*P(word2|C)*...*P(wordn|C)*P(C)
            // P(C) = N(C) / N

            double probPhraseGivenPositive = 1;
            foreach (string s in content)
            {
                if (vocabulary.ContainsKey(s))
                {
                    double probWordGivenPositive = vocabularyPositive[s] / (double)vocabulary[s];
                    probPhraseGivenPositive *= probWordGivenPositive != 0 ? probWordGivenPositive : 0.10; // we don't want to multiply by zero
                }
            }

            double probPhraseGivenNegative = 1;
            foreach (string s in content)
            {
                if (vocabulary.ContainsKey(s))
                {
                    double probWordGivenNegative = Math.Abs(vocabularyPositive[s] / (double)vocabulary[s] - 1);
                    probPhraseGivenNegative *= probWordGivenNegative != 0 ? probWordGivenNegative : 0.10; // we don't want to multiply by zero
                }
            }

            double N = reviews.Count;
            double NPos = reviews.Count(r => r.Positive);
            double NNeg = reviews.Count(r => !r.Positive);
            double probabilityPositive = NPos / N;
            double probabilityNegative = NNeg / N;

            double probReviewGivenPositive = probPhraseGivenPositive * probabilityPositive;
            double probReviewGivenNegative = probPhraseGivenNegative * probabilityNegative;

            // normalize
            double NormalizedProbReviewGivenPositive =
                probReviewGivenPositive / (probReviewGivenPositive + probReviewGivenNegative);
//                double NormalizedProbReviewGivenNegative =
//                    probReviewGivenNegative / (probReviewGivenPositive + probReviewGivenNegative);

            return NormalizedProbReviewGivenPositive;
        }


        public static void PerformCuts()
        {
            List<User> users = Parser.ParseUsers("friendships.reviews.txt");

            Console.WriteLine("number of users: " + users.Count);

            DenseMatrix userAdjMatrix = Utils.ConstructAdjMatrix(users);
            DenseMatrix userDegMatrix = Utils.ConstructDegMatrix(users);

            // compute unnormalized laplacion L
            DenseMatrix laplacianMatrix = userDegMatrix - userAdjMatrix;
            Evd<double> m = laplacianMatrix.Evd(); // matrix decomposition

            // compute second eigenvector of L
            DenseVector secondEigenvector = DenseVector.OfArray(m.EigenVectors.ToColumnArrays()[1]);

            // associate users and eigenvector values
            List<Utils.Pair<User, double>> usersEigenPairs = new List<Utils.Pair<User, double>>();
            for (int i = 0; i < users.Count; i++)
            {
                usersEigenPairs.Add(new Utils.Pair<User, double>(users[i], secondEigenvector[i]));
            }

            // order nodes according to eigenvector value
            usersEigenPairs.Sort(new Utils.SecondPairComparer<User>());

            // List gaps with indexes and sort them by gap size so the largest gaps are in the top
            List<Utils.Pair<int, double>> gapIndexPairs = new List<Utils.Pair<int, double>>();
            for (int i = 0; i < usersEigenPairs.Count - 1; i++)
            {
                gapIndexPairs.Add(new Utils.Pair<int, double>(i + 1,
                    Math.Abs(usersEigenPairs[i].Second - usersEigenPairs[i + 1].Second)));
            }

            gapIndexPairs.Sort(new Utils.SecondPairComparer<int>());
            gapIndexPairs.Reverse();

            // cut a untill we have communities of length 2 to 10
            for (int numCuts = 1; numCuts < 10; numCuts++)
            {
                // get numCuts indexes to cut at and sort
                List<int> IndexesToCut =
                    gapIndexPairs.GetRange(0, numCuts).Select(p => p.First).OrderBy(i => i).ToList();
                List<List<User>> communities = new List<List<User>>();
                int indexProgress = 0;
                foreach (int index in IndexesToCut)
                {
                    communities.Add(usersEigenPairs.GetRange(indexProgress, index - indexProgress).Select(p => p.First)
                        .ToList());
                    indexProgress = index;
                }

                communities.Add(usersEigenPairs.GetRange(indexProgress, usersEigenPairs.Count - indexProgress)
                    .Select(p => p.First).ToList());

                // write communities to file
                string[] communityStrings = new string[numCuts + 1];
                for (int i = 0; i < communities.Count; i++)
                {
                    string namesInCommunity = "";
                    List<User> community = communities[i];
                    var arr = community.Select(u => u.Name).ToArray();
                    foreach (string name in arr)
                    {
                        namesInCommunity += name + " ";
                    }

                    communityStrings[i] = namesInCommunity.TrimEnd();
                }

                File.WriteAllLines("./communities/" + numCuts.ToString() + "cut", communityStrings);
            }
        }
    }
}