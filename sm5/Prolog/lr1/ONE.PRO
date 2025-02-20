domains
    person = symbol
database
    marriedDB(person,person)
    siblingsDB(person, person)
predicates
    male(person)
    female(person)
    married(person,person)
    son(person, person)
    daughter(person, person)
    mom(person, person)
    dad(person, person)
    sis(person, person)
    bro(person, person)
    uncle(person, person)
    aunt(person, person)
    granny(person, person)
    granddaddy(person, person)
    kuzen(person, person)
    kuzina(person, person)
    plemyannik(person, person)
    plemyannitsa(person, person)
    testb(person, person)
    tesha(person, person)
    svekr(person, person)
    svekrov(person, person)
    parent(person, person)
    predok(person,person)
    relative(person, person)
    siblings(person, person)
    start()
    startNot()

clauses
    male(tixon).
    male(vladimir).
    male(aleksandr).
    male(aleksandre).
    male(valya).
    male(jacob).
    male(georgiy).
    male(grigoriy).
  
    female(aida).
    female(aglaya).
    female(julia).
    female(lesya).
    female(sonya).

    dad(vladimir, tixon).
    dad(vladimir, jacob).
    dad(vladimir, sonya).
    dad(aleksandr, valya).
    dad(aleksandre, julia).
    dad(aleksandre, aleksandr).
    dad(grigoriy, lesya).
    dad(grigoriy, vladimir).

    mom(aglaya, lesya).
    mom(aglaya, vladimir).
    mom(aida, julia).
    mom(aida, aleksandr).
    mom(julia, tixon).
    mom(julia, jacob).
    mom(julia, sonya).
    mom(julia, georgiy).
    mom(lesya, valya).

    startNot() :- married(X,Y),fail; siblings(Z,A),fail.
    start() :- not(startNot()).
    
    son(X, Y) :- male(X), dad(Y,X); male(X), mom(Y,X).
    daughter(X, Y) :- female(X), dad(Y,X); female(X), mom(Y,X).

    married(X, Y) :- female(Y), male(X), mom(Y, Z), dad(X, Z), not(marriedDB(X, Y)), assert(marriedDB(X,Y)); marriedDB(X,Y).

    parent(X, Y) :- dad(X, Y); mom(X, Y).

    sis(X, Y) :- female(X), mom(SOME, X), mom(SOME, Y), X<>Y.
    bro(X, Y) :- male(X), mom(SOME, X), mom(SOME, Y), X<>Y.
    siblings(X,Y) :- parent(SOME, X), parent(SOME, Y), X<>Y, not(siblingsDB(X,Y)), assert(siblingsDB(X,Y)); siblingsDB(X,Y).      %bro or sister exist

    uncle(X, Y) :- male(X), bro(BRO, X), dad(BRO, Y); male(X), sis(SIS, X), mom(SIS, Y). 
    aunt(X, Y) :- female(X), sis(X, BRO), dad(BRO, Y); female(X), sis(X, SIS), mom(SIS, Y).

    granny(X, Y) :- parent(SOME, Y), mom(X, SOME).
    granddaddy(X, Y) :- parent(SOME, Y), dad(X, SOME).

    kuzen(X, Y) :- male(X), parent(PARENT, X), siblings(PARENT, SOME), parent(SOME, Y), X<>Y.
    kuzina(X, Y) :- female(X), parent(PARENT, X), siblings(PARENT, SOME), parent(SOME, Y), X<>Y.

    plemyannik(X, Y) :- male(X), parent(PARENT, X), siblings(PARENT, Y).
    plemyannitsa(X, Y) :- female(X), parent(PARENT, X), siblings(PARENT, Y).

    testb(X, Y) :- male(X), male(Y), married(Y, SOME), dad(X, SOME).
    tesha(X, Y) :- female(X), male(Y), married(Y, SOME), mom(X, SOME).

    svekr(X, Y) :-  male(X), female(Y), married(SOME, Y), dad(X, SOME).
    svekrov(X, Y) :- female(X), female(Y), married(SOME, Y), mom(X, SOME).

    predok(X, Y) :- parent(X, Y), X<>Y.
    predok(X, Y) :- parent(X,Z), parent(Z,Y),X<>Y.

    relative(X,Y) :- siblings(X,Y).
    relative(X,Y) :- predok(X,Y); predok(Y,X),X<>Y.
    relative(X,Y) :- predok(Z,X),predok(Z,Y),X<>Y,Z<>X,Z<>Y.
    relative(X,Y) :- predok(Z,X),predok(A,Y),siblings(Z,A),X<>Y,Z<>X,Z<>Y,A<>X,A<>Y.