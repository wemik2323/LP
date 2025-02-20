%trace
domains
	list = integer*

predicates
	readSize(integer)
	checkSize(integer)
	powOfTwo(integer, integer)
	inputList(integer, list)
	printList(list)
	split(integer, list, list, list)
	listFirst(integer, list, list, list)
	replace_at_index(integer,integer,list,list)
	get_element(integer,list,integer)
	swap(integer, integer, list, list)
	runway(integer,integer, list,list)
	loop(integer,integer,list,list)

clauses
	readSize(X) :- 
		clearwindow,
		write("Enter value "),nl,
		readint(X),!;
		readSize(X).
		
	checkSize(X) :-
		X > 0;
		write("Negative power"),!.

	powOfTwo(0, 1).
	powOfTwo(X, RES) :-
		X > 0, 
		X1 = X - 1,
		powOfTwo(X1, TempRES),
		RES = TempRES * 2.
	
	inputList(0, []).
	inputList(Size, [H|T]) :-
		clearwindow,
		write(Size), write(" values left"),nl,
		readint(H),
		Size1=Size-1,
		inputList(Size1, T).
		
	split(Size,[H|T],Left,Right) :-
		Half = Size / 2,
		listFirst(Half, [H|T], Left, Right).
	
	listFirst(0, Tail, [], Tail).
	listFirst(Half, [H|T], [H|Tl], Tail) :-
		Half1 = Half - 1,
		listFirst(Half1, T, Tl, Tail).
		
	swap(Index1,Index2,List,NewList) :-
		get_element(Index1, List, Value1), 
		get_element(Index2, List, Value2),
		replace_at_index(Index1, Value2, List, TempList),
		replace_at_index(Index2, Value1, TempList, NewList).
		
		
	replace_at_index(0, NewValue, [_|Tail], [NewValue|Tail]).
	replace_at_index(Index, NewValue, [Head|Tail], [Head|NewTail]) :- 
		Index > 0, 
		NewIndex = Index - 1, 
		replace_at_index(NewIndex, NewValue, Tail, NewTail).

	get_element(0, [Head|_], Head).
	get_element(Index, [_|Tail], Element) :- 		
		Index > 0, 	
		NewIndex = Index - 1, 
		get_element(NewIndex, Tail, Element).

	runway(-1,_,List,List).
	runway(Iters, Offset, List, Out) :-
	
		NewIters = Iters - 1,
		Index1 = Iters,
		Index2 = Index1 + Offset,
%		write(Index1),
%		write(Index2),nl,
		get_element(Index1,List,Val1),
		get_element(Index2,List,Val2),
%		write(Val1),write(" "),
%		write(Val2),write(" "),nl,
		Val1 <= Val2,
%		write(Offset), write(" "), write(Iters),nl,
%		readchar(_),
		runway(NewIters, Offset, List, Out);

		NewIters = Iters - 1,
		Index1 = Iters,
		Index2 = Index1 + Offset,
%		write(Index1),nl,
%		write(Index2),nl,
		get_element(Index1,List,Val1),
		get_element(Index2,List,Val2),
%		write(Val1),write(" "),
%		write(Val2),write(" "),nl,
		Val1 > Val2,
%		write(Offset), write(" "), write(Iters),nl,
%		readchar(_),
		swap(Index1,Index2,List,NewList),
		runway(NewIters, Offset, NewList, Out).

	loop(Size, 0.5, List, Out) :-
		Iters = Size - 2,
		runway(Iters, 1, List, Out).
	loop(Size, Offset, List, Out) :-
		Iters = Size - Offset - 1,
		runway(Iters, Offset, List, TempList),
		NewOffset = Offset/2,
		loop(Size,NewOffset,TempList,Out).

		
	printList([]).
	printList([H|T]) :-
		write(H), write(" "),
		printList(T).
		
goal
	clearwindow,
	%runway(6, 1, [1,2,3,4,8,7,6,5], Out),
	%loop(8, 4, [8,7,6,5,1,2,3,4], Out),
	%printList(Out).
	readSize(Size),
	checkSize(Size),
	powOfTwo(Size, RES),
	%write(RES).
	inputList(RES, List),
	clearwindow,
	Offset = RES / 2,
	loop(RES,Offset,List,NewList),
	printList(NewList).