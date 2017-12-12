loves(rose, jack).
loves(jack, rose).
loves(caledon, rose).
happy(X) :- loves(X, Y), loves(Y, X).

