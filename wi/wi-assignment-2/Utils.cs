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
        
        public class MutableKeyValuePair<KeyType, ValueType>
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
        
        public class UserEigenPairComparer : IComparer<MutableKeyValuePair<User, double>>
        {
            public int Compare(MutableKeyValuePair<User, double> x, MutableKeyValuePair<User, double> y)
            {
                if (x.Value > y.Value)
                    return 1;
                else if (x.Value < y.Value)
                    return -1;
                return 0;
            }
        }
    }
}