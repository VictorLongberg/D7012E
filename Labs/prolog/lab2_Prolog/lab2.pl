%Victor Longberg, viclon-8

%********************************Help Functions********************************

% Sum
% sum_list([1,2,3],X)
sum_list([],0).
sum_list([H|T],Sum) :- 
    sum_list(T,Sum2),
    Sum is H + Sum2.

%sort
sort_list(List,Result) :- msort(List,Result).

%drop - removes i items from list. Int -> [a] -> [a]
%drop(2,[1,2,3,4],X).
drop(_,[],[]).
drop(0,L,L).
drop(1,[_|Tail],Tail) :- !.
drop(K,[_|Tail],Result) :-
    K > 1,
    K2 is K - 1,
    drop(K2,Tail,Result).

%take - takes the j items from the list. Int -> [a] -> [a]
%take(2,[1,2,3,4],X).
take(_,[],[]).
take(0,L,L).
take(1,[Head|_],[Head]):- !.
take(K,[X|Xs],[X|Ys]) :-
    K > 1,
    K2 is K - 1,
    take(K2,Xs,Ys).

%tail all but first elements.
%tail ([1,2,3],X).
tail([_|Tail],Tail).

%head first elements.
%head([1,2,3],X).
head([Head|_],Head).

%******************************Calculate Sublist**********************************

calcAllSublists(List, Result) :-
    listIndexes(List, IndexList),
    calcAllSublistsIndex(List,IndexList, [], SortedSubList),
    sort_list(SortedSubList, Result).

calcAllSublistsIndex(_, [], Temp, Temp).
calcAllSublistsIndex(List, [Head|Tail], Temp, Result) :-
    calcSublist(Head, List, Calc),
    append(Temp, [Calc], Rara),
    calcAllSublistsIndex(List,Tail,Rara,Result).

%calcSublist
%calcSublist([1,3],[1,2,3,4],X).
calcSublist([I|J],List,Result) :- 
    head(J, J2),
    I2 is I - 1,
    J3 is J2 - I2,
    drop((I2),List,DR),
    take((J3),DR,Sublist),
    sum_list(Sublist, Sum),
    Result = [Sum,I,J2,Sublist].

%****************************Finding Indexes************************************

%Creates array of index combinations
listIndexes(List,Result) :-
    length(List,Len),
    range(1,Len,RangeList), %create a list from the range of the list.
    findall(Y, (perm(2,RangeList,X) ,sort_list(X,Y)), Ys), %create all permutations of that list, and sort it.
    sort(Ys, Result). %sort it again to remove duplicates.

range(I,I,[I]).
range(I,K,[I|L]) :- I < K, I1 is I + 1, range(I1,K,L).

%I create all permutations for the list of size 2.
perm(1, Input, [Last]) :-
    member(Last, Input).
perm(N, Input, [First,Second|Perm]) :- 
    N > 1, N0 is N-1,
    member(First, Input),
    perm(N0, Input, [Second|Perm]).

%*********************************Printing****************************************

smallest_k(List,K,_) :-
    calcAllSublists(List,Sublists),
    take(K,Sublists,TakenSubLists),
    write("[size,i,j,[sublist]]"),
    print_smallest_k(TakenSubLists).
    
print_smallest_k([]).
print_smallest_k([Head|Tails]) :-
    write("\n"),
    write(Head),
    print_smallest_k(Tails).

test0() :- smallest_k([-1,2,-3,4,-5],3,_).
test1() :- smallest_k([-1,2,-3,4,-5,6,-7,8,-9,10,-11,12,-13,14,-15,16,-17,18,-19,20,-21,22,-23,24,-25,26,-27,28,-29,30,-31,32,-33,34,-35,36,-37,38,-39,40,-41,42,-43,44,-45,46,-47,48,-49,50,-51,52,-53,54,-55,56,-57,58,-59,60,-61,62,-63,64,-65,66,-67,68,-69,70,-71,72,-73,74,-75,76,-77,78,-79,80,-81,82,-83,84,-85,86,-87,88,-89,90,-91,92,-93,94,-95,96,-97,98,-99,100],15,_).
test2() :- smallest_k([24,-11,-34,42,-24,7,-19,21],6,_).
test3() :- smallest_k([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],8,_).
