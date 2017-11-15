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
            // With the current spectral cut algorithm, it only makes sense to cut twice. The rest of the cuts only a few people are shaven of a community each cut
//            List<List<User>> communities = Parser.ParseCommunitites(users, 2);

//            Utils.PrintCommunities(communities);
//            Exercises.Exercise2();
            
            Console.WriteLine();
            Console.WriteLine("-------------------------------");
            Console.WriteLine("Execution time: " + (DateTime.Now - startTime));
        }
        
        
        
        
        
        
        
        
        
        
        
        
        
        public static void PerformCuts()
        {
            List<User> users = Parser.ParseUsers("friendships.reviews.txt");

//            List<User> users = new List<User>
//            {
//                new User("per", new HashSet<string>(){"asger", "mads"},null,null),
//                new User("asger", new HashSet<string>(){"per", "mads"},null,null),
//                new User("mads", new HashSet<string>(){"per", "asger", "anne", "marie"},null,null),
//                new User("anne", new HashSet<string>(){"mads", "marie", "søren", "katja"},null,null),
//                new User("marie", new HashSet<string>(){"mads", "anne", "søren", "katja"},null,null),
//                new User("søren", new HashSet<string>(){"anne", "marie", "katja", "simon"},null,null),
//                new User("katja", new HashSet<string>(){"anne", "marie", "søren", "simon"},null,null),
//                new User("simon", new HashSet<string>(){"søren", "katja", "morten"},null,null),
//                new User("morten", new HashSet<string>(){"simon"},null,null),
//            };
            
            Console.WriteLine("number of users: " + users.Count);
            
            DenseMatrix userAdjMatrix = Utils.ConstructAdjMatrix(users);
            DenseMatrix userDegMatrix = Utils.ConstructDegMatrix(users);
            
            // compute unnormalized laplacion L
            DenseMatrix laplacianMatrix = userDegMatrix - userAdjMatrix;
            Evd<double> m = laplacianMatrix.Evd();
            
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
                List<int> IndexesToCut = gapIndexPairs.GetRange(0, numCuts).Select(p => p.First).OrderBy(i => i).ToList();
                List<List<User>> communities = new List<List<User>>();
                int indexProgress = 0;
                foreach (int index in IndexesToCut)
                {
                    communities.Add(usersEigenPairs.GetRange(indexProgress, index - indexProgress).Select(p => p.First).ToList());
                    indexProgress = index;
                }
                communities.Add(usersEigenPairs.GetRange(indexProgress, usersEigenPairs.Count - indexProgress).Select(p => p.First).ToList());
            
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
                File.WriteAllLines("./communities/" + numCuts.ToString() +  "cut", communityStrings);
            }
        }
    }
}