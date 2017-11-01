﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using MathNet.Numerics;
using MathNet.Numerics.LinearAlgebra;
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
            Console.WriteLine(m.EigenValues);
//            Console.WriteLine(m.D);
//            Console.WriteLine(m.Determinant);
            Console.WriteLine(m.EigenVectors);
//            Console.WriteLine(m.IsFullRank);
//            Console.WriteLine(m.IsSymmetric);
//            Console.WriteLine(m.Rank);
        }

        public static void Exercise2()
        {
            MutableKeyValuePair<string, double>[] wordsArray =
            {
                new MutableKeyValuePair<string, double>("good", 0),
                new MutableKeyValuePair<string, double>("I", 0),
                new MutableKeyValuePair<string, double>("like", 0),
                new MutableKeyValuePair<string, double>("this", 0),
                new MutableKeyValuePair<string, double>("shit", 0),
                new MutableKeyValuePair<string, double>("amazing", 0),
                new MutableKeyValuePair<string, double>("awesome", 0),
                new MutableKeyValuePair<string, double>("horrible", 0),
                new MutableKeyValuePair<string, double>("is", 0),
                new MutableKeyValuePair<string, double>("don't", 0)
            };
            Review[] reviews = 
            {
                new Review(true, "this is good"),
                new Review(false, "this is bad"),
                new Review(true, "this is amazing"),
                new Review(false, "this is shit"),
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
                new Review(false, "is shit"),
                new Review(false, "is horrible"),
                new Review(true, "is awesome"),
                new Review(true, "is amazing"),
                new Review(false, "this is bad shit"),
            };
            
            for (int i = 0; i < wordsArray.Length; i++)
            {
                double containCount = 0, posCount = 0;
                var word = wordsArray[i];
                for (int j = 0; j < reviews.Length; j++)
                {
                    var review = reviews[j];
                    if (review.Content.Contains(word.Key))
                    {
                        containCount++;
                        if (review.Positive)
                        {
                            posCount++;
                        }
                    }
                }
                word.Value = posCount / containCount;
            }
            
            string content = "good amazing this is";
            Console.WriteLine(content);
            Console.WriteLine(Review.probPos(content, reviews, wordsArray));
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

            public override string ToString()
            {
                return Positive + ": " + Content;
            }

            static public double probPos(string content, Review[] reviews, MutableKeyValuePair<string, double>[] words)
            {
                // N = number of reviews
                // P(Review|pos) = P(word1|pos)*P(word2|pos)*...*P(wordn|pos)*P(pos)
                // P(pos) = N(pos) / N
                double wordProbs = 1;
                foreach (string s in content.Split(' '))
                {
                    wordProbs *= words.First(w => w.Key == s).Value + 1;
                }

                double NPos = reviews.Count(r => r.Positive);
                double N = reviews.Length;
                wordProbs = wordProbs * (NPos / N);

                return wordProbs;
            }
        }
        
        private class MutableKeyValuePair<KeyType, ValueType>
        {
            public KeyType Key { get; set; }
            public ValueType Value { get; set; }

            public MutableKeyValuePair() { }

            public MutableKeyValuePair(KeyType key, ValueType val)
            {
                Key = key;
                Value = val;
            }
        }
    }
}