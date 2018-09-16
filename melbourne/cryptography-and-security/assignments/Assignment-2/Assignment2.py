


### Question 1
from assignment1 import gcd, modInverse
# p = 4841247740021026788214420074996258540545281
# q = 712010411572858151605922429225626518528001
# n = p*q
# phiN = (p-1)*(q-1)
#
# # calculate lowest possible public key e
# e = 2
# while gcd(e, phiN) != 1:
#     e += 1
#
# # calculate private key d for e
# d = modInverse(e, phiN)


### Question 2
# https://medium.com/@prudywsh/how-to-generate-big-prime-numbers-miller-rabin-49e6e6af32fb
# from prime_generator import generate_prime_number
from Crypto.PublicKey import RSA
from Crypto import Random

# help(RSA)
# dir(RSA)





# m = bytearray(b'some message')
# print(m)
# for i in m:
#     print(i)



# m = 'some Message'
# ml = int(m)
# print(ml)
# m2 = str(ml)
# print(m2)

# t = int("thisisamessage", 36)
# print(t)
# s = str(t)
# m = 'some Message'.encode('utf8')
# m = 'some Message'.encode('hex')
# print(m)
# # m = m**5
#
# random_generator = Random.new().read
#
# key = RSA.generate(1024, random_generator)
# c = key.encrypt(m, 32)
#
# print(c)
#
# p = key.decrypt(c)
#
# print(p.decode('utf8'))

import utils
# intStr = utils.strToInt("aaasome message")
# p = utils.intToStr(intStr)
# print(p)

p = 178319560416421126516098795404787923479198619991288369991746803226075823896339226395566468236370956822016607309099932912303039705347464214350980280894546880615142891784801668100442228738495697185461381092319342853078600104786027875103868303407168893783849097704473719421290876791688821228860711202827309622833
q = 133109942917198594159406349294359373733546098091030389432472367131217900290553510582625465777379198366192187042985885832333281342526706040385972208598858593941536206415608373548171241328041012871719775599152762109642472483461096474354215264651302948899923781085195238509480572970321476953371643652687065106617
n = p*q
# print(n)
phiN = (p-1)*(q-1)
# print(phiN)

e = 2
while gcd(e, phiN) != 1:
    e += 1

d = modInverse(e, phiN)

# print(e)
# print(d)

def E(m, e, n):
    return (m**e)%n
def D(c, d, n):
    return (c**d)%n

m = utils.strToInt("here we gooooooo!")
print(m)
c = E(m, e, n)
print(d)
print(c)
# p = D(c, d, n)
# print(p)
# m2 = utils.intToStr(p)
# print(m2)




### Question 3


