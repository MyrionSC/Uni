import numpy as np

def main():

    plaintexts = ["CTRL", "CAPS", "HOME", "PGUP"]
    ciphertexts = ["JYZP", "QEPQ", "CHZS", "GLXF"]
    plainVectors = np.arange(16).reshape(4,4)
    cipherVectors = np.arange(16).reshape(4,4)

    for i in range(0, len(plaintexts)):
        for j in range(0, len(plaintexts[i])):
            plainVectors[i][j] = cToI(plaintexts[i][j])
            cipherVectors[i][j] = cToI(ciphertexts[i][j])


    print(plainVectors)
    print(cipherVectors[0])


    b = np.array([cipherVectors[0][0], cipherVectors[1][0], cipherVectors[2][0], cipherVectors[3][0]])
    print(b)

    x = np.linalg.solve(plainVectors, b)
    print(x)




    # for pair in plainCipherPairs:
    #     print(pair)











    # msg = np.array([0, 2, 19])  # CAT
    # key = np.matrix([
    #     [6, 24, 1],
    #     [13, 16, 10],
    #     [20, 17, 15]
    # ])
    #
    # ciphertext = np.mod(np.dot(key, msg), 26)  # POH
    #
    # invKey = modMatInv(key, 26)
    # ciphertext = ciphertext.reshape(3, 1)
    #
    # plaintext = np.mod(np.dot(invKey, ciphertext), 26)
    # print(plaintext)



# modulo matrix inverse
def modMatInv(A,p):       # Finds the inverse of matrix A mod p
  n=len(A)
  A=np.matrix(A)
  adj=np.zeros(shape=(n,n))
  for i in range(0,n):
    for j in range(0,n):
      adj[i][j]=((-1)**(i+j)*int(round(np.linalg.det(minor(A,j,i)))))%p
  return (modInv(int(round(np.linalg.det(A))),p)*adj)%p
def modInv(a,p):          # Finds the inverse of a mod p, if it exists
  for i in range(1,p):
    if (i*a)%p==1:
      return i
  raise ValueError(str(a)+" has no inverse mod "+str(p))
def minor(A,i,j):    # Return matrix A with the ith row and jth column deleted
  A=np.array(A)
  minor=np.zeros(shape=(len(A)-1,len(A)-1))
  p=0
  for s in range(0,len(minor)):
    if p==i:
      p=p+1
    q=0
    for t in range(0,len(minor)):
      if q==j:
        q=q+1
      minor[s][t]=A[p][q]
      q=q+1
    p=p+1
  return minor

# translators
def cToI (c):
    if (c == ","):
        return 26
    if (c == "."):
        return 27
    if (c == " "):
        return 28
    return ord(c) - 65
def iToC (i):
    if (i == 26):
        return ","
    if (i == 27):
        return "."
    if (i == 28):
        return " "
    return chr(65 + i)





if __name__ == '__main__':
    main()










