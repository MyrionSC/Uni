import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
import io







A = [
    [1, 1, 1],
    [0, 2, 5],
    [2, 5, -1]
]

b = [6, -4, 27]

# X = np.linalg.solve(A, b)
ainv = np.invert(A)
X = np.dot(ainv, b)

print(X)

# AX=B

# x + y + z = 6
# 2y + 5z = -4
# 2x + 5y - z = 27