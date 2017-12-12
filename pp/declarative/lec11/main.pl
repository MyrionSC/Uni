nul :- z.
en :- s(z).
to :- s(s(z)).
tre :- s(s(s(z))).
fire :- s(s(s(s(z)))).

%Problem 1. Write a Prolog program that does not terminate.

base(const).
rec(X) :- rec(X), base(X).

%Problem 2. Write a Prolog program that cannot be expressed in Datalog.

nat(x).
nat(s(X)) :- nat(X).

%The herbrandt universe is infinite, so it cannot be expressed in datalog

%Problem 3. Write a Prolog program that use an unsafe fact.

loves(rose, X).
%    It is unsafe because there is a variable in a base rule

%Problem 4. The natural numbers are defined as:
%Nat(Z).
%Nat(S(x)) ⇐ Nat(x).
%Implement the following relations on natural numbers: +, −, ×, ≤, and minimum.

add(X, z, X).
add(z, Y, Y).
add(s(X), Y, s(R)) :- add(X, Y, R).

sub(X, z, X).
sub(z, Y, Y).
sub(s(X), s(Y), R) :- sub(X, Y, R).

mult(X, z, z) :- nat(X).
mult(z, Y, z) :- nat(Y).
mult(X, s(z), X) :- nat(X).
mult(s(z), Y, Y) :- nat(Y).
mult(s(X), Y, R) :- mult(X, Y, W), add(Y, W, R).

lteq(z, Y) :- nat(Y).
lteq(s(X), s(Y)) :- lteq(X, Y).

mini(X, Y, X) :- lteq(X, Y), nat(Y), nat(X).
mini(X, Y, Y) :- lteq(Y, X), nat(Y), nat(X).

%Problem 5. Describe how Prolog enables computation of subtraction from addition.
sub2(X, Y, R) :- add(Y, R, X).

%Problem 6. Use Prolog to determine if the following equations have a solution:
%• x = 1 + 2
%add(s(z), s(s(z)), R).
%• x + 2 = 3
%add(X, s(s(z)), s(s(s(z)))).
%• x × x = 3
%• x × x + 1 = 26
%• x ≤ minimum(x, y)
%where x and y are natural numbers.
%Problem 7. Implement Odd(n) and Even(n) to determine if a number is odd or even.
%Problem 8. Implement the Fibonacci function.


