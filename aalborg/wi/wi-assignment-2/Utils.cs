using System;
using System.Collections.Generic;
using MathNet.Numerics.LinearAlgebra.Double;

namespace wi_assignment_2
{
    public class Utils
    {
        public static DenseMatrix ConstructDegMatrix(List<User> users)
        {
            double[,] userDegArr = new double[users.Count, users.Count];
            for (int i = 0; i < users.Count; i++)
            {
                userDegArr[i, i] = users[i].Friends.Count;
            }
            return DenseMatrix.OfArray(userDegArr);
        }
        public static DenseMatrix ConstructAdjMatrix(List<User> users)
        {
            double[,] userAdjArr = new double[users.Count, users.Count];
            for (int i = 0; i < users.Count; i++)
            {
                User user = users[i];
                for (int j = 0; j < users.Count; j++)
                {
                    User targetUser = users[j];
                    if (user != targetUser && user.Friends.Contains(targetUser.Name))
                    {
                        userAdjArr[i, j] = 1;
                    }
                    else
                    {
                        userAdjArr[i, j] = 0;
                    }
                }
            }
            return DenseMatrix.OfArray(userAdjArr);
        }
        
        public class Pair<T1, T2>
        {
            public T1 First { get; set; }
            public T2 Second { get; set; }

            public Pair() { }
            public Pair(T1 key, T2 val)
            {
                First = key;
                Second = val;
            }
        }
        
        
        public static void PrintCommunities(List<List<User>> communities)
        {
            Console.WriteLine();
            for (int i = 0; i < communities.Count; i++)
            {
                List<User> c = communities[i];
                Console.WriteLine("community " + (i + 1) + " count: " + c.Count);
            }
        }
        
        public class SecondPairComparer<T> : IComparer<Pair<T, double>>
        {
            public int Compare(Pair<T, double> x, Pair<T, double> y)
            {
                if (x.Second > y.Second)
                    return 1;
                else if (x.Second < y.Second)
                    return -1;
                return 0;
            }
        }
        public class FirstPairComparer<T> : IComparer<Pair<int, T>>
        {
            public int Compare(Pair<int, T> x, Pair<int, T> y)
            {
                if (x.First > y.First)
                    return 1;
                else if (x.First < y.First)
                    return -1;
                return 0;
            }
        }
    }
}