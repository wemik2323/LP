%trace
constants
	n=0
	h=0.01

domains
	sp = real*
	odds = sp*
predicates
	readSize(integer)
	KoshiValid(integer,sp)
	inputList(integer,sp)
	inputOdds(integer,integer, odds)
	printOdds(integer, odds)
	k1(sp,sp,real)
	k2(sp,sp,sp,real)
	allK1(odds,sp,sp)
	allK2(odds,sp,sp,sp)
	allK(integer,odds,sp,sp,odds)
	mult(real,real,real,real)
	printList(sp)
	fark4(integer,integer,odds,sp)
	func4(integer,integer,sp,odds,sp)
	indexList(integer,sp,real)
	answer(integer,real,sp,real)
	k26(sp,sp,sp,real)
	allK26(odds,sp,sp,sp)
	mult26(real,real,real,real)
	k36(sp,sp,sp,real)
	allK36(odds,sp,sp,sp)
	mult36(real,real,real,real)
	k46(sp,sp,sp,sp,sp,real)
	allK46(odds,sp,sp,sp,sp,sp)
	mult46(real,real,real,real,real,real)
	k56(sp,sp,sp,sp,sp,real)
	allK56(odds,sp,sp,sp,sp,sp)
	mult56(real,real,real,real,real,real)
	k66(sp,sp,sp,sp,sp,sp,sp,real)
	allK66(odds,sp,sp,sp,sp,sp,sp,sp)
	mult66(real,real,real,real,real,real,real,real)
	answer6(sp,sp,sp,sp,sp,sp,sp,sp)
	rk6(odds,sp,sp)
	conc(sp,sp,sp)
	rev(sp,sp)
	merge(sp,odds,odds)
	start()
clauses

	conc([],X,X).
	conc([H|T],X,[H|Z]):-conc(T,X,Z).
 
	rev([],[]).
	rev([H|T],Z) :- rev(T,Q), conc(Q,[H],Z).

	start() :-
		readSize(SIZE),
		KoshiValid(SIZE,L_Koshi),
		inputOdds(SIZE,SIZE,M),
		allK1(M,L_Koshi,K1),
		allK(4,M,L_Koshi,K1,M_K),
		merge(K1,M_K,M_K4),
		printOdds(4,M_K4),
		func4(SIZE,4,L_Koshi,M_K4,RES4),
		rev(RES4,RES4rev),
		write("Answer 4:"),nl,
		printList(RES4rev),nl,
		rk6(M,L_Koshi,RES6),
		write("Answer 6:"),nl,
		printList(RES6),nl.
			
	readSize(X) :- 
		clearwindow,
		write("BBeDNTe chNc/\\o"),nl,
		readint(X),!;
		readSize(X).
	
	KoshiValid(Size,K) :-
		clearwindow,
		write("BBedNTe yc/\\oBNE KolllN"),nl,
		inputList(Size,K),!;
		clearwindow,
		write("Error while reading list"),
		KoshiValid(Size,K).
		
	inputList(0,[]).
	inputList(Size,[H|T]) :-
		readreal(H),
		Size1=Size-1,
		inputList(Size1,T).
	
	inputOdds(_, 0, []) :- !.
	inputOdds(Size, Xsize,[H|T]) :-
		clearwindow,
		Current = Size - Xsize + 1,
		write("BBedNTe ypaBHeHNe - "),
		write(Current),nl,
		inputList(Size,H),
		SIZE1=Xsize-1,
		inputOdds(Size,SIZE1,T);
		inputOdds(Size,Xsize,[H|T]).
	
	printOdds(0,_) :- !.
	printOdds(SIZE,[H|T]) :-
		write(H),nl,
		SIZE1=SIZE-1,	
		printOdds(SIZE1,T).
	
	printList([]).
	printList([H|T]) :-
		write(H),write(" "),
		printList(T).	
		
	k1([],[],0).
	k1([HK|TK],[HN|TN],ANS) :- 
		TMP = HK * HN,
		k1(TK,TN,SUM),
		ANS = TMP + SUM.
	
	allK1([],_,[]).
	allK1([H_koef|T_koef],Koshi,[Hk|Tk]) :-
		k1(H_koef,Koshi,Hk),
		allK1(T_koef,Koshi,Tk).
		
	k2([],[],[],0).	
	k2([H_koef|T_koef], [Hn|Tn], [Hk|Tk], ANS) :-
		mult(H_koef,Hn,Hk,MUL),
		k2(T_koef,Tn,Tk,SUM),
		ANS = MUL + SUM. 

	allK2([],_,_,[]).
	allK2([H_koef|T_koef], Koshi, K1, [Hk2|Tk2]) :-
		k2(H_koef,Koshi,K1,Hk2),
		allK2(T_koef,Koshi,K1,Tk2).
			
	allK(0,_,_,_,[]).
	allK(Size,[H_koef|T_koef], Koshi, K1, [K2|Tk]) :-
		allK2([H_koef|T_koef],Koshi,K1,K2),
		Size1=Size-1,
		allK(Size1,[H_koef|T_koef], Koshi, K2, Tk).
		
	mult(HK,HN,K,ANS) :-
		ANS = HK * (HN + 0.5 * h * K). 	
		
	indexList(1,[X|_],X) :- !.
	indexList(N,[_|T],Y) :-
	        N1 = N-1,
		indexList(N1,T,Y).

	fark4(0,_,_,[]) :- !.
	fark4(Size,Index,[H|T],[Hr|Tr]) :-
		indexList(Index,H,Hr),
		Size1=Size-1,
		fark4(Size1,Index,T,Tr).
		
	func4(0,_,[],_,[]) :- !.
	func4(Index, P, [H_Koshi|T_Koshi], [H|T], [ANS|Ta]) :-
		fark4(P, Index, [H|T], ListK),
		answer(P, H_Koshi, ListK, ANS),
		Ind = Index - 1,
		func4(Ind,P,T_Koshi,[H|T], Ta).
		
	answer(4,X0,[K1,K2,K3,K4],ANS) :-
		ANS=X0*(h/6)*(K1+2*K2+2*K3+K4).	
		
	k26([],[],[],0).	
	k26([H_koef|T_koef], [Hn|Tn], [Hk|Tk], ANS) :-
		mult26(H_koef,Hn,Hk,MUL),
		k26(T_koef,Tn,Tk,SUM),
		ANS = MUL + SUM. 

	allK26([],_,_,[]).
	allK26([H_koef|T_koef], Koshi, K1, [Hk2|Tk2]) :-
		k26(H_koef,Koshi,K1,Hk2),
		allK26(T_koef,Koshi,K1,Tk2).

	mult26(HK,HN,K,ANS) :-
		ANS = h*HK*(HN + 0.25 * K).

	k36([],[],[],0).	
	k36([H_koef|T_koef], [Hn|Tn], [Hk1|Tk1], ANS) :-
		mult36(H_koef,Hn,Hk1,MUL),
		k36(T_koef,Tn,Tk1,SUM),
		ANS = MUL + SUM. 

	allK36([],_,_,[]).
	allK36([H_koef|T_koef], Koshi, K1, [Hk3|Tk3]) :-
		k36(H_koef,Koshi,K1,Hk3),
		allK36(T_koef,Koshi,K1,Tk3).

	mult36(HK,HN,K1,ANS) :-
		ANS = h*HK*(HN + (1/2)*K1).

	k46([],[],[],[],[],0).	
	k46([H_koef|T_koef], [Hn|Tn],[Hk1|Tk1],[Hk2|Tk2], [Hk3|Tk3],ANS) :-
		mult46(H_koef,Hn,Hk1,Hk2,Hk3,MUL),
		k46(T_koef,Tn,Tk1,Tk2,Tk3,SUM),
		ANS = MUL + SUM. 

	allK46([],_,_,_,_,[]).
	allK46([H_koef|T_koef], Koshi,K1,K2, K3, [Hk4|Tk4]) :-
		k46(H_koef,Koshi,K1,K2,K3,Hk4),
		allK46(T_koef,Koshi,K1,K2,K3,Tk4).

	mult46(HK,HN,K1,K2,K3,ANS) :-
		ANS = h*HK*(HN + (1/7)*K1 + (2/7)*K2 + (1/14)*K3).

	k56([],[],[],[],[],0).	
	k56([H_koef|T_koef], [Hn|Tn], [Hk1|Tk1],[Hk3|Tk3],[Hk4|Tk4],ANS) :-
		mult56(H_koef,Hn,Hk1,Hk3,Hk4,MUL),
		k56(T_koef,Tn,Tk1,Tk3,Tk4,SUM),
		ANS = MUL + SUM.

	allK56([],_,_,_,_,[]).
	allK56([H_koef|T_koef], Koshi,K1,K3,K4, [Hk5|Tk5]) :-
		k56(H_koef,Koshi,K1,K3,K4,Hk5),
		allK56(T_koef,Koshi,K1,K3,K4,Tk5).

	mult56(HK,HN,K1,K3,K4,ANS) :-
		ANS = h * HK*(HN + (3/8)*K1 - (1/2)*K3 + (1/14)*K4).

	k66([],[],[],[],[],[],[],0).	
	k66([H_koef|T_koef], [Hn|Tn], [Hk1|Tk1], [Hk2|Tk2],[Hk3|Tk3],[Hk4|Tk4],[Hk5|Tk5],ANS) :-
		mult66(H_koef,Hn,Hk1,Hk2,Hk3,Hk4,Hk5,MUL),
		k66(T_koef,Tn,Tk1,Tk2,Tk3,Tk4,Tk5,SUM),
		ANS = MUL + SUM. 

	allK66([],_,_,_,_,_,_,[]).
	allK66([H_koef|T_koef], Koshi, K1, K2, K3, K4, K5, [Hk6|Tk6]) :-
		k66(H_koef,Koshi,K1,K2,K3,K4,K5,Hk6),
		allK66(T_koef,Koshi,K1,K2,K3,K4,K5,Tk6).

	mult66(HK,HN,K1,K2,K3,K4,K5,ANS) :-
		ANS = h * HK * (HN - (4/7)*K1 + (12/7)*K2 - (2/7)*K3 - K4 + (8/7)*K5).

	answer6(_,[],[],[],[],[],[],[]) :- !.
	answer6([H_Koshi|T_Koshi],[Hk1|Tk1],[Hk2|Tk2],[Hk3|Tk3],[Hk4|Tk4],[Hk5|Tk5],[Hk6|Tk6],[Hans|Tans]) :-
		Hans = H_Koshi + (7/90)*(Hk1+Hk6)+(16/45)*(Hk2+Hk5)-Hk3/3+7*Hk4/15,
		answer6(T_Koshi,Tk1,Tk2,Tk3,Tk4,Tk5,Tk6,Tans).

	rk6(Mk,Koshi,ANS) :-
		allK1(Mk,Koshi,K1),
		allK26(Mk,Koshi,K1,K2),
		allK36(Mk,Koshi,K1,K3),
		allK46(Mk,Koshi,K1,K2,K3,K4),
		allK56(Mk,Koshi,K1,K3,K4,K5),
		allK66(Mk,Koshi,K1,K2,K3,K4,K5,K6),
		answer6(Koshi,K1,K2,K3,K4,K5,K6,ANS).
				
	merge(K1,K,[Hr|Tr]) :-
		Hr = K1,
		Tr = K.
		
goal
	clearwindow,
	start().
	