using System;
using System.Collections.Generic;
using System.IO;
using MathNet.Numerics;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Double;
using MathNet.Numerics.LinearAlgebra.Factorization;

namespace wi_assignment_2
{
    public class Exercise
    {
        public static void exercise1()
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
    }
}