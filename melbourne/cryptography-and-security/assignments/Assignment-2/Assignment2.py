


### Question 1
# from assignment1 import gcd, modInverse
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
# from assignment1 import gcd, modInverse
# # prime_generator from: https://medium.com/@prudywsh/how-to-generate-big-prime-numbers-miller-rabin-49e6e6af32fb
# from prime_generator import generate_prime_number, generate_prime_candidate
#
# ## On Alice side: Generate new rsa key
# p = generate_prime_number(1024)# log(p) -> 308.17534441586166
# q = generate_prime_number(1024) # log(q) -> 308.2001243895489
# n = p*q # log(n) -> 616.3754094820681
# phiN = (p-1)*(q-1) # log(phiN) -> 616.3754094820681
# e = 2
# while gcd(e, phiN) != 1:
#     e += 1
# # e -> 5
# d = modInverse(e, phiN) # log(d) -> 615.9774694733961
#
# # Borrowing the powers of Crypto.PublicKey.RSA because unblinding is taking too long with
# # the length of the numbers involved
# from Crypto.PublicKey import RSA
# key = RSA.construct((n, e, d, p, q))
# pub = key.publickey()
#
# ## On Bobs side: Create message and blinding factor r. Blind message.
# import utils # Simple helper for converting string to int
# m = utils.strToInt("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur justo nulla, lobortis at tincidunt in, convallis sit amet nibh. Etiam vulputate diam ullamcorper volutpat pulvinar. ")
# # log(m) -> 549.0318571168225
# r = generate_prime_candidate(1024)
# while gcd(r, n) != 1:
#     r = generate_prime_candidate(1024)
# # log(r) -> 308.17882904526067
# m_blinded = pub.blind(m, r) # log(m_blinded) -> 615.8406352867902
#
# ## On Alices side: m_blinded received from Bob, sign m_blinded
# m_blinded_signature = key.sign(m_blinded, 0)[0] # log(m_blinded_signature) -> 615.143894636624
#
# ## On Bobs side: m_blinded_signature received from Alice. Unblind to get Alices signature for m
# m_signature = pub.unblind(m_blinded_signature, r) # log(m_signature) -> 616.1771166060526
#
# ## Verify that the signature is actually Alices signature
# m_direct_signature = key.sign(m, 32)[0] # log(m_direct_signature) -> 616.1771166060526
# print(m_signature == m_direct_signature) # True
# print(pub.verify(m, (m_signature,))) # True
# print(pub.verify(m, (m_direct_signature,))) # True


### Question 3







# import utils
# def H(M):
#     return sum([x**2 for x in M]) % 928374928374
# M = utils.strToIntArray("TOM MARVOLO RIDDLE")
# M2 = utils.strToIntArray("I AM LORD VOLDEMORT")
# print(H(M) == H(M2)) # one










