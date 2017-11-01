using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
            List<User> users = new List<User>()
            {
                new User("per", new HashSet<string>(){"asger", "mads"},null,null),
                new User("asger", new HashSet<string>(){"per", "mads"},null,null),
                new User("mads", new HashSet<string>(){"per", "asger", "anne", "marie"},null,null),
                new User("anne", new HashSet<string>(){"mads", "marie", "søren", "katja"},null,null),
                new User("marie", new HashSet<string>(){"mads", "anne", "søren", "katja"},null,null),
                new User("søren", new HashSet<string>(){"anne", "marie", "katja", "simon"},null,null),
                new User("katja", new HashSet<string>(){"anne", "marie", "søren", "simon"},null,null),
                new User("simon", new HashSet<string>(){"søren", "katja", "morten"},null,null),
                new User("morten", new HashSet<string>(){"simon"},null,null),
            };
            
            
            Console.WriteLine("number of users: " + users.Count);
            
            DenseMatrix userAdjMatrix = Utils.ConstructAdjMatrix(users);
            DenseMatrix userDegMatrix = Utils.ConstructDegMatrix(users);
            
            // compute unnormalized laplacion L
            DenseMatrix laplacianMatrix = userDegMatrix - userAdjMatrix;
            Evd<double> m = laplacianMatrix.Evd();
            
            // compute second eigenvector of L
            DenseVector secondEigenvector = DenseVector.OfArray(m.EigenVectors.ToColumnArrays()[1]);

            // associate users and eigenvector values
            List<Utils.MutableKeyValuePair<User, double>> usersEigenPairs = new List<Utils.MutableKeyValuePair<User, double>>();
            for (int i = 0; i < users.Count; i++)
            {
                usersEigenPairs.Add(new Utils.MutableKeyValuePair<User, double>(users[i], secondEigenvector[i]));
            }
            
            // order nodes according to eigenvector value
            usersEigenPairs.Sort(new Utils.UserEigenPairComparer());
            
            // calculate cut at the largest gap // todo perform more cuts at the same time
            int largestGapIndex = 0;
            double largestGap = 0;
            for (int i = 0; i < usersEigenPairs.Count - 1; i++)
            {
                double gap = Math.Abs(usersEigenPairs[i].Value - usersEigenPairs[i + 1].Value);
                if (gap > largestGap)
                {
                    largestGap = gap;
                    largestGapIndex = i;
                }
            }
            
            // perform cut and create communities
            var leftCut = usersEigenPairs.GetRange(0, largestGapIndex + 1);
            var rightCut = usersEigenPairs.GetRange(largestGapIndex + 1, usersEigenPairs.Count - largestGapIndex - 1);
            
            // todo convert pairs to Users
//            List<List<User>> communities = 
            
            
            
            
            
            
            

            Console.WriteLine(largestGap);
            Console.WriteLine(largestGapIndex);




//            Exercises.Exercise1();
//            Console.WriteLine(m.EigenValues);

            
            
            Console.WriteLine();
            Console.WriteLine("-------------------------------");
            Console.WriteLine("Execution time: " + (DateTime.Now - startTime));
        }
    }
}