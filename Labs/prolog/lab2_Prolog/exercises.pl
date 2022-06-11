%https://sites.google.com/site/prologsite/prolog-problems

%1.01, (*) Find the last element of a list. 
my_last(X,[X]). %if its only one item
% matches the tail to the X value, | creates head and tail
my_last(X,[_|Z]) :- last(X,Z). 

%1.03, The first element in the list is number 1.
%30.00 ish
%https://www.youtube.com/watch?v=CSXHnWuIrGY&list=PL_aTLqb10zrLLiA-BL_UcOs8A91dHAN8F&index=14

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- 
    K > 1,
    K2 is K-1,
    element_at(X,L,K2).

%1.04 Finds size
% https://www.youtube.com/watch?v=xl7NzdcgqXo

size([],0).
size([_|T],N) :- 
    size(T,N1),
    N is N1 +1.

%1.05 reverse list
% http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse25


accRev([H|T],A,R):-  accRev(T,[H|A],R).
%First we take  away head from L
%Insert head into A,
%and pass R as Accumulator
accRev([],A,A). %When list is empty we return the rec stack.
%When list is empty R = A.

%rev([1,2,3],X). List, Result. Uses accumulator
rev(L,R) :- accRev(L,[],R).

%1.06 Palindrome. Trivial with rev
%We insert a list (P) we want to see if its a palindrome.
%What we doe is reverse the Palindrome list resulting in the answer R.
%We check if the argument for pali P is equal to the output of rev(R)

% pali(["race car"]).
pali(P) :- rev(P,R), R = P. 

%1.08 remove duplicates.
%1hour 6min ish.
%https://www.youtube.com/watch?v=CSXHnWuIrGY&list=PL_aTLqb10zrLLiA-BL_UcOs8A91dHAN8F&index=14

compress([],[]).
compress([X],[X]).
compress([X,X|Xs],Ys) :- compress([X | Xs], Ys). %when first two are equal
compress([X,Y|Ys2], [X|Zs]) :- 
    X \= Y, %if X cannot be unified with Y, They are different
    compress([Y|Ys2],Zs).


%1.14 duplicate
dupli([],[]).
dupli([X],[X,X]).
dupli([X|XS],[X,X|YS]) :- dupli(XS,YS). 

%1.20 remove kth element Similar to element at.
remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- 
    K > 1, 
    K1 is K - 1, 
    remove_at(X,Xs,K1,Ys).
 
%1.21 insert
%remove_at(X,[a,b,c],2,R).
%remove_at(b,[a,b,c],2,R).
insert_at(X,L,K,R) :- remove_at(X,R,K,L). %monkaS

%1.22 range finns i håkan video...





