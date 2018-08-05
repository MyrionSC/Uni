import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
import io

# linear regression with one independent variable

# CSV file with variables YEAR,TIME
csv = """1896,4.47083333333333
1900,4.46472925981123
1904,5.22208333333333
1908,4.1546786744085
1912,3.90331674958541
1920,3.5695126705653
1924,3.8245447722874
1928,3.62483706600308
1932,3.59284275388079
1936,3.53880791562981
1948,3.6701030927835
1952,3.39029110874116
1956,3.43642611683849
1960,3.2058300746534
1964,3.13275664573212
1968,3.32819844373346
1972,3.13583757949204
1976,3.07895880238575
1980,3.10581822490816
1984,3.06552909112454
1988,3.09357348817
1992,3.16111703598373
1996,3.14255243512264
2000,3.08527866650867
2004,3.1026582928467
2008,2.99877552632618
2012,3.03392977050993"""

olympics = np.genfromtxt(io.BytesIO(csv.encode()), delimiter=",")
# print(olympics)

time = olympics[:, 0:1]
year = olympics[:, 1:2]

x_test = np.arange(1890, 2020)[:, None]

# def plot_fit(x_test, y_test, x, y):
#     plt.plot(x_test, y_test, 'b-')
#     plt.plot(x, y, 'rx')
#     plt.ylabel("y (Race time)")
#     plt.xlabel("x (Year of race)")
#     plt.show()

# print(2)
#
plt.plot(time, year, 'rx')
plt.ylabel("y (Race time)")
plt.xlabel("x (Year of race)")
plt.show()
#
#
# ### gradiant descent
#
# w1 = -0.4
# n = len(olympics)
#
# def update_w0(x, y, w1):
#     return np.sum((y - w1 * x)) / len(x)
#
# w0 = update_w0(time, year, w1)
# print(w0)



# def update_w1(x, y, w0):
#     return np.sum((y - w0) * x) / np.sum(x**2)
#
# w1 = update_w1(x, y, w0)
# print(w1)



### linear algebra solution

# print(time)
#
# ones = np.ones_like(time)
# print(ones)

X = np.hstack((np.ones_like(time), time))
# print(X)

w = ((X.T.dot(X))**-1).dot(X.T).dot(year)
print(w)

w0, w1 = w

lr = LinearRegression().fit(time, year)

print(lr)


# plot_fit(x_test, predict(x_test, w0, w1), x, y)

#w∗=[X⊤X]−1X⊤y

# w = np.linalg.solve







