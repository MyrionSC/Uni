using System;
using System.Linq;
using MathNet.Numerics.LinearAlgebra.Double;
using MathNet.Numerics.LinearAlgebra.Factorization;

namespace wi_assignment_2
{
    public class Exercises
    {
        public static void Exercise1()
        {
            // degree matrix
            DenseMatrix degMatrix = DenseMatrix.OfArray(new double[,]
            {
                {2, 0, 0, 0, 0, 0},
                {0, 2, 0, 0, 0, 0},
                {0, 0, 3, 0, 0, 0},
                {0, 0, 0, 3, 0, 0},
                {0, 0, 0, 0, 2, 0},
                {0, 0, 0, 0, 0, 2}
            });

            // adjacancy matrix
            DenseMatrix adjMatrix = DenseMatrix.OfArray(new double[,]
            {
                {0, 1, 1, 0, 0, 0},
                {1, 0, 1, 0, 0, 0},
                {1, 1, 0, 1, 0, 0},
                {0, 0, 1, 0, 1, 1},
                {0, 0, 0, 1, 0, 1},
                {0, 0, 0, 1, 1, 0}
            });
            // laplacian
            DenseMatrix laplacianMatrix = degMatrix - adjMatrix;

            Console.WriteLine("Laplacian matrix");
            Console.WriteLine(laplacianMatrix);

            Console.WriteLine();
            Console.WriteLine("--------------------------");
            Console.WriteLine();

            Evd<double> m = laplacianMatrix.Evd();
            Console.WriteLine();
//            Console.WriteLine(m.D);
//            Console.WriteLine(m.Determinant);
            Console.WriteLine(DenseVector.OfArray(m.EigenVectors.ToColumnArrays()[1]));
//            Console.WriteLine(m.IsFullRank);
//            Console.WriteLine(m.IsSymmetric);
//            Console.WriteLine(m.Rank);
        }

        public static void Exercise2()
        {
            Utils.Pair<string, double>[] vocabulary =
            {
                new Utils.Pair<string, double>("good", 0),
                new Utils.Pair<string, double>("I", 0),
                new Utils.Pair<string, double>("like", 0),
                new Utils.Pair<string, double>("this", 0),
                new Utils.Pair<string, double>("drivel", 0),
                new Utils.Pair<string, double>("amazing", 0),
                new Utils.Pair<string, double>("awesome", 0),
                new Utils.Pair<string, double>("horrible", 0),
                new Utils.Pair<string, double>("is", 0),
                new Utils.Pair<string, double>("don't", 0)
            };
            Review[] reviews = 
            {
                new Review(true, "this is good"),
                new Review(false, "this is bad"),
                new Review(true, "this is amazing"),
                new Review(false, "this is drivel"),
                new Review(true, "this is awesome"),
                new Review(false, "this is horrible"),
                new Review(true, "I like this"),
                new Review(false, "I don't like this"),
                new Review(true, "I like amazing"),
                new Review(true, "I like awesome"),

                new Review(false, "I don't like"),
                new Review(true, "I like awesome"),
                new Review(true, "this good"),
                new Review(true, "is good"),
                new Review(false, "is bad"),
                new Review(false, "is drivel"),
                new Review(false, "is horrible"),
                new Review(true, "is awesome"),
                new Review(true, "is amazing"),
                new Review(false, "this is drivel"),
            };
            
            for (int i = 0; i < vocabulary.Length; i++)
            {
                double containCount = 0, posCount = 0;
                Utils.Pair<string, double> word = vocabulary[i];
                for (int j = 0; j < reviews.Length; j++)
                {
                    var review = reviews[j];
                    if (review.Content.Contains(word.First))
                    {
                        containCount++;
                        if (review.Positive)
                        {
                            posCount++;
                        }
                    }
                }
                word.Second = posCount / containCount;
            }
            
            string test1 = "good amazing this is";
            string test2 = "amazing horrible drivel";
            Console.WriteLine(test1 + ": " + Review.contentPositiveProbability(test1, reviews, vocabulary));
            Console.WriteLine(test2 + ": " + Review.contentPositiveProbability(test2, reviews, vocabulary));
        }
        
        
        private class Review
        {
            public bool Positive;
            public string Content;

            public Review(bool positive, string content)
            {
                Positive = positive;
                Content = content;
            }

            static public double contentPositiveProbability(string content, Review[] reviews,
                Utils.Pair<string, double>[] vocabulary)
            {
                // N = number of reviews
                // P(Review|C) = P(word1|C)*P(word2|C)*...*P(wordn|C)*P(C)
                // P(C) = N(C) / N
                
                double probPhraseGivenPositive = 1;
                foreach (string s in content.Split(' '))
                {
                    double probWordGivenPositive = vocabulary.First(w => w.First == s).Second;
                    probPhraseGivenPositive *= probWordGivenPositive;
                }
                
                double probPhraseGivenNegative = 1;
                foreach (string s in content.Split(' '))
                {
                    double probWordGivenNegative = Math.Abs(vocabulary.First(w => w.First == s).Second - 1);
                    probPhraseGivenNegative *= probWordGivenNegative;
                }

                double N = reviews.Length;
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
        }
    }
}