likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

friends(X, X) :- !, fail.
friends(X, Y) :- likes(X, Z), likes(Y, Z).