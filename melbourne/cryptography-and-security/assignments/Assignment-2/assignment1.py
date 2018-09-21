
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
