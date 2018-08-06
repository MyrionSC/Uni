import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
import io



A = [
    [1, 1],
    [0, 2],
    [0, 2]
]

print(np.dot(np.transpose(A), A))

print(np.linspace(0,1,1000))

# A = [
#     [1, 1],
#     [0, 2]
# ]
#
# b = [6, -4]
#
# X = np.linalg.solve(A, b)
# print(X)

# AX=b

# x + y + z = 6
# 2y + 5z = -4
# 2x + 5y - z = 27