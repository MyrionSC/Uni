god(odin).
son(odin, thor).
son(odin, baldr).
son(thor, mothi).
son(thor, magni).
demiGod(X) :- son(Y, X), god(Y).
mortal(X) :- son(Y, X), demiGod(Y).
