woman(vincent).
woman(mia).
man(jules).
father(methusalah, mia).

person(X) :- man(X); woman(X).

loves(X,Y) :- knows(Y,X).

father(Y,Z) :- man(Y),son(Z,Y).
father(Y,Z) :- man(Y),daughter(Z,Y).