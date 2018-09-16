
def euclidean(a, b):
    c = a % b
    return b if c == 0 else euclidean(b, c)

def extendedEuclideanAlgorithm(a, b):
    if a != 0:
        gcd, x, y = extendedEuclideanAlgorithm(b % a, a)
        return (gcd, y - (b // a) * x, x)
    else:
        return (b, 0, 1)

def modInverse(a, n):
    gcd, x, _ = extendedEuclideanAlgorithm(a, n)
    if gcd == 1:
        return x % n
    return None

gcd = euclidean


#
# P = 20
# a, b = 3,4
#
# def E(a, b, P):
#     return (a*P+b) % 26
#
# def D(a, b, C):
#     return (C-b) * (modInverse(a, 26)) % 26
#
# C = E(a,b,P)
# print(C)
#
# PP = D(a, b, C)
# print(PP)



# p = 5
# a = 2
#
# phi = p**a - p**(a-1)
#
# print(phi)









# a, b = 996622688165337716830009929239, 716029746093661370396795904119

# p1, p2 = 996622688165337716830009929239, 716029746093661370396795904119
# n = p1 * p2 # 713611490358209018128116052497523311086662789934136218635441
#
# i = modInverse(291357, n) # 553456279592608982823045707330732906913618060715331682618277
# ii = modInverse(p1 * 4, n) # None
# iii = modInverse(n-1, n) # 713611490358209018128116052497523311086662789934136218635440
# print(i)
# print(ii)
# print(iii)
