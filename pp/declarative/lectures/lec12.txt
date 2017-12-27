
%Problem 1. Name a few Datalog and Prolog engines.


%Problem 2. When would you use Datalog to solve a problem? And Prolog?

    % When I need some prolog only functionality like recursive rules or variables in the atoms then I would use prolog, otherwise datalog

%A binary tree of natural numbers can be defined as:
tree(leaf).
tree(node(L, N, R)) :- integer(N),tree(L),tree(R).

%Problem 3. Assume the tree is unsorted, compute if it contains a given number.

%searchNum(tree(N), N).
searchNum(node(L, N, R), N) :- integer(N), tree(L), tree(R).
searchNum(node(L, N, R), X) :- searchNum(L, X).
searchNum(node(L, N, R), X) :- searchNum(R, X).

%Problem 4. Assume the tree is sorted, compute if it contains a given number.

bs(node(L, N, R), N) :- integer(N), tree(L), tree(R).
bs(node(L, N, R), X) :- X < N, bs(L, X).
bs(node(L, N, R), X) :- X > N, bs(R, X).

%Problem 5. Compute the minimum and maximum height of a tree.
treeHeight(leaf, 1).
treeHeight(node(L, N, R), H) :- treeHeight(L, Y), H is Y + 1.
treeHeight(node(L, N, R), H) :- treeHeight(R, Y), H is Y + 1.
maxHeight(T, X) :- treeHeight(T, X), \+((treeHeight(T, Y), Y > X)).

%Problem 6. Compute the sum of the elements of a tree.
treeSum(leaf, 0).
treeSum(node(L, N, R), S) :- treeSum(L, X), treeSum(R, Y), S is X+Y+N.
%treeSum(node(node(leaf, 100, leaf), 10, node(leaf, 20, node(leaf, 30, leaf))), S). -> 160

%roblem 7. Translate a tree to a list using a pre-, in-, and post-order traversal.
treeListPre(leaf, []).
treeListPre(node(L, N, R), Res) :- treeListPre(L, X), treeListPre(R, Y), append([X, [N], Y], Res).
treeListIn(leaf, []).
treeListIn(node(L, N, R), Res) :- treeListIn(L, X), treeListIn(R, Y), append([[N], X, Y], Res).
treeListPost(leaf, []).
treeListPost(node(L, N, R), Res) :- treeListPost(L, X), treeListPost(R, Y), append([Y, [N], X], Res).

%Problem 8. The following definition of Remove for lists is incorrect. Fix it:
%Remove(x, [], []).
%Remove(x, [x|ys],rs) ⇐ Remove(x, ys,rs).
%Remove(x, [y|ys],rs) ⇐ Remove(x, ys,rs).
%and describe what was wrong.


