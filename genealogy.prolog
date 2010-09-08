male(adam).
female(eve).
female(lilith).

son(adam,eve,cain).
son(adam,eve,abel).
son(adam,eve,seth).
daughter(adam,eve,frances).

daughter(adam,lilith,fluffy).
son(adam,lilith,amos).

son(abel,fluffy,ed).
son(adam,fluffy,kevin).


parents(M,F,C):-son(M,F,C).
parents(M,F,C):-daughter(M,F,C).

male(C):-son(_,_,C).
female(C):-daughter(_,_,C).
        
parent(M,C):-parents(M,_,C).
parent(F,C):-parents(_,F,C).

mother(M,C):-parent(M,C),female(M).
father(M,C):-parent(M,C),male(M).

mother(M):-mother(M,_).
father(F):-father(F,_).

son(S,P):-father(P,S),male(S).
son(S,P):-mother(P,S),male(S).
daughter(S,P):-father(P,S),female(S).
daughter(S,P):-mother(P,S),female(S).



son(S):-son(S,_).
daughter(D):-daughter(D,_).

siblings(A,B):-parent(P,A),parent(P,B),A\=B.
full_siblings(A,B):-mother(M,A),mother(M,B),father(F,A),father(F,B),A\=B,M\=F.

cousins(A,B):-parent(P,A),parent(P2,B),siblings(P,P2).