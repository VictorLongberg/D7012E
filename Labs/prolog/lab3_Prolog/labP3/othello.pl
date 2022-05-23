/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Victor Longberg
%    Student user id  : viclon-8@student.ltu.se 
%
/* ------------------------------------------------------- */

:- ensure_loaded('play.pl'). %do not change.

:- [testboards].

%:- ensure_loaded('stupid.pl'). %Remove comment to enable stupid bot to have it play for you.

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 

%located in testboards.pl.

%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :- testBoard300(InitialState), InitialPlyr = 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr) :- 
	terminal(State), 				  %Winner is not declared before a terminal state is reached.
	calculateScore(1,State,0,Score1), %Calculates the score for respective player.
	calculateScore(2,State,0,Score2),
	Score1 \= Score2, 		% making sure we dont declare a winner incase of tie.
    ((Score1 < Score2) -> %person with least points/stones wins. 	
        Plyr = 1
    ; 
        Plyr = 2
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- 
	terminal(State),
	calculateScore(1,State,0,Score1), % If scores are equal
	calculateScore(2,State,0,Score2), % the game is a tie.
	Score1 = Score2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State):- %should both be true or only one?
	moves(1, State, []), 
	moves(2, State, []).  % checking if the movelist is null / empty.
						  % if so were at a terminal state of the game.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State

moves(Plyr, State, MvList) :-
	length(State,Len),
	Index is Len -1, 			%length of a 6x6 is 6, but we index from 0 so -1.
	range(0,Index,RangeList), 	% returns [0,1,2,3,4,5]
	findall(Y,perm(2,RangeList,Y),CoordList), % find all combinations of 2set [X,Y] coordinates.
	calculateTestmoves(Plyr, State, CoordList, [], Moves),	% Calculate all possible moves.
	sort(Moves, Moves2),  		%Sort it to remove possible duplicates and so on..
	%append(Moves2, [['n']], Moves3),
	tail(Moves2,MvList).  		%remove the first element since its always a empty list.
	%writeln(MvList).


calculateTestmoves(_,_,[],Temp,Temp). 						% For the list of Cordinates run its
calculateTestmoves(Plyr,State,[Head|Tails],Temp,Moves) :- 	% length to find all possible movies.
	testmoves(Plyr,State,Head,SoFar),						% Returns the cordinates if they are valid.
	append(Temp,[SoFar],SoFar2),							% Append the list to total MvList
	calculateTestmoves(Plyr,State,Tails,SoFar2,Moves).

testmoves(Plyr,State,[X,Y],SoFar) :-
	(validmove(Plyr, State, [X,Y]) % will fail if we get false, so i add empty array when false,
	-> 							   % so I sort the resulting array in the end to remove all empty arrays
		SoFar = [X,Y]			   % and then remove the head of that array containing the last empty array.
	;
		SoFar = []
	).

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).

nextState(Plyr, 'n', State, State, NextPlyr) :- opponent(Plyr, NextPlyr). %if null/n is chosen we still change player
nextState(Plyr, Move, State, NewState, NextPlyr) :- 
	opponent(Plyr,NextPlyr),
	get(State, Move, '.'),			    % Check so that the move is on a dot
	moves(Plyr, State, MoveList),	    % Make sure that the move is contained
	member(Move,MoveList),		        % within in the list of possible moves.
	set(State, TempState1, Move, Plyr), % Set the Move input to the player value.
	findAllFlips(Plyr, TempState1, Move, NewState).
	%NextPlyr = Opponent,
	%NewState = FinalTempState.

% Find the Combinations of flips available in the newstate.
% Takes each state from the previous function and applies it to the next and so on.
findAllFlips(Plyr, State, Move, Result) :- 
	north([XN,YN]), south([XS,YS]), east([XE,YE]), west([XW,YW]),
	northeast([XNE,YNE]), northwest([XNW,YNW]), southeast([XSE,YSE]), southwest([XSW,YSW]),
	findFlip(Plyr, State, Move, Move, [XN|YN], TempState2),
	findFlip(Plyr, TempState2, Move, Move, [XS|YS], TempState3),
	findFlip(Plyr, TempState3, Move, Move, [XE|YE], TempState4),
	findFlip(Plyr, TempState4, Move, Move, [XW|YW], TempState5),
	findFlip(Plyr,TempState5, Move, Move, [XNE|YNE], TempState6),
	findFlip(Plyr,TempState6, Move, Move, [XNW|YNW], TempState7),
	findFlip(Plyr,TempState7, Move, Move, [XSE|YSE], TempState8),
	findFlip(Plyr,TempState8, Move, Move, [XSW|YSW], Result).
	
%I find out if theres any distance X between the origin [X1|Y1] untill the end of board where
%Theres two of the same Player type (1 or 2), if that is true we go into flipped.
%Flipped simply changes all values inbetween to the same value as the player.


findFlip(Plyr, TempState, [X1|Y1], [X2|Y2], [X3|Y3], Result) :-
	opponent(Plyr, Opponent),  %Opponent 
	X4 is X2 + X3,	%Opponent cordinates			  
	Y4 is Y2 + Y3,
	X5 is X2 + X3 + X3,	%Player Cordinates.
	Y5 is Y2 + Y3 + Y3,
	((get(TempState,[X4,Y4],Opponent)), (validCords([X2,Y2])),(validCords([X4,Y4])),(validCords([X5,Y5])) -> %We make sure that the next step is a opponent
		((get(TempState,[X4,Y4],Opponent)),(get(TempState,[X5,Y5],Plyr)) -> 
			X6 is X1 + X3,
			Y6 is Y1 + Y3,
			flipped(Plyr, TempState, [X6|Y6], [X3|Y3], Result)
		;
			findFlip(Plyr, TempState, [X1|Y1], [X4|Y4], [X3|Y3],Result)
		)
	;
		Result = TempState
	).

%Flipps all the values untill it finds one where the get value is not true
%Then i return the state result.

							%set  %Direction
flipped(Plyr, TempState, [X|Y], [X2|Y2], Result) :-
	opponent(Plyr,Opponent),
	((get(TempState,[X,Y],Opponent)) ->
		set(TempState, TempState2, [X,Y], Plyr),
		X3 is X + X2,
		Y3 is Y + Y2,
		flipped(Plyr, TempState2, [X3|Y3], [X2|Y2], Result)
	;
		Result = TempState
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

/*
	    	 North
	NorthWest	  NoerthEast
West		  				East
	SouthWest	  SouthEast
			 South
*/

% We test if we can move in any of the 8 directions.
% and if that case is ever true then that cordinate is valid.
validmove(Plyr, State, Proposed) :- 
	get(State, Proposed, '.'), 		% we want to find the occurances of "." and in the 
	move(Plyr, State, Proposed).	% case we find a "." we want to find out if theres
							   		% a possible combination for that dot be be moved to.
%%%% North, South, East, West. %%%%

move(Plyr, State, [X, Y]) :- 
	north([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	south([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	east([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	west([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

%%%% NorthEast, NorthWest, SouthEast, SouthWest %%%%

move(Plyr, State, [X, Y]) :- 
	northeast([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	northwest([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	southwest([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

move(Plyr, State, [X, Y]) :- 
	southeast([X2,Y2]),
	trymove(Plyr, State, [X, Y], [X2, Y2]).

% If we get a opponent then the player we have succesfully found
% a path of which a empty spot followed by X opponents and then the player
% meaning its a available move.

trymove(Plyr, State, [X,Y], [X2,Y2]) :-
	X3 is X+X2,  %Opponent coordinate
	Y3 is Y+Y2,
	validCords([X3,Y3]),
	X4 is X3+X2, %Player coordinate
	Y4 is Y3+Y2,
	validCords([X4,Y4]),
	opponent(Plyr, Opponent),
	get(State, [X3,Y3],Opponent),
	((get(State, [X3,Y3], Opponent)),(get(State,[X4,Y4], Plyr)) -> 
		!
		;
		X5 is X + X2,
		Y5 is Y + Y2,
		get(State,[X5,Y5],Opponent),
		trymove(Plyr,State,[X5,Y5],[X2,Y2])
	).

%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%
%constant coordinates values.
north([0,-1]).
south([0,1]).
east([1,0]).
west([-1,0]).

northeast([1,-1]).
northwest([-1,-1]).
southeast([1,1]).
southwest([-1,1]).

%returns your opponents number.
opponent(1, 2).
opponent(2, 1).

%a way to check that you are using valid cords
validCords([X,Y]) :- X > -1, X < 6, Y > -1, Y < 6. %Hardcoded board size, could change 6 to length of board.

% Taken from lab2.pl

%Returns the tail of a list
tail([_|Tail],Tail).

%provides a range list.
range(I,I,[I]).
range(I,K,[I|L]) :- I < K, I1 is I + 1, range(I1,K,L).

%creates permutations of list input for size 2 lists.
perm(1, Input, [Last]) :-
    member(Last, Input).
perm(N, Input, [First,Second|Perm]) :- 
    N > 1, N0 is N-1,
    member(First, Input),
    perm(N0, Input, [Second|Perm]).

%%%%%%%%%%%%%%%%%%%%%%%%% calculateScore %%%%%%%%%%%%%%%%%%%%%%%%%
%% given player,State,Temp returns result score for that player.

%counts the amount of reocurrences of 1 or 2 and adds the amount of times.
calculateScore(_,[],Temp,Temp). %When list is empty the Temp variable is set to result.
calculateScore(Plyr, [Head|Tail], Temp, Result) :-
	count(Plyr, Head, Score),
	Score2 is Score + Temp,
	calculateScore(Plyr,Tail,Score2,Result).

count(_, [], 0).
count(X, [X | T], N) :-
!, count(X, T, N1),
N is N1 + 1.
count(X, [_ | T], N) :-
count(X, T, N). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

% part 2 in asg4-with-some-annotations....
% inspired from ttt.pl

h(State,100) :- winner(State,1), !.
h(State,-100) :- winner(State,2), !.
h(State,0) :- tie(State), !.
h(_,0).

%%%%%%%%%%%%%%%%%%%%%%%%% lowerBound(B) & upperBound(B) %%%%%%%%%%%%%%%%%%%%%%%%%

%https://www.rose-hulman.edu/class/cs/csse513/papers/7-search.pdf
%på pdf sida 23 så förklaras de ish?. 2^(6+1) -1. 

%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(B) :- B = -127.

%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(B) :- B = 127.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get(Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test functions %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Calculate Score %%
test0() :- initBoard(InitialState), calculateScore(1, InitialState, 0, R), writeln("occurances = " + R).
test01() :- initBoard(InitialState), calculateScore(2, InitialState, 0, R), writeln("occurances = " + R).

%% Length %%
test1() :- initBoard(InitialState), length(InitialState, Int), write(Int).

%% Sort %% 
test2() :- sort([[1,1],[0,0],[2,7],[0,2],[1,5]], R), write(R).

%% Winner & Tie %% 
test3() :- initBoard(InitialState), winner(InitialState,R), write(R).
test4() :- initBoard(InitialState), tie(InitialState).

%% Range, IndexList %%
test5() :- range(1,5,R), write(R).
test6() :- findall(Y,perm(2,[0,1,2,3,4,5],Y),YS), write(YS).

%% Moves %%
test7() :- initBoard(InitialState), moves(1,InitialState,R), write(R).
test8() :- initBoard(InitialState), moves(2,InitialState,R), write(R).
test9() :- testBoardMoves(InitialState), moves(1,InitialState,R), write(R).
test13() :- initBoard2(InitialState), moves(2,InitialState,R), write(R).
test14() :- initBoard2(InitialState), moves(1,InitialState,R), write(R).
test142() :- initBoard3(InitialState), moves(1,InitialState,R), write(R).
test143() :- initBoard4(InitalState), moves(1,InitalState,R), write(R).
test144() :- initBoard5(InitalState), moves(2, InitalState,R), write(R).
test145() :- initBoard6(InitalState), moves(1,InitalState,R), write(R).
test148() :- initBoard8(InitalState), moves(2,InitalState,R), write(R).

%get
test10() :- get([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[2,3],2).
test11() :- get([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[2,2],1).
test20() :- initBoard3(InitialState), get(InitialState, [2,1], R), writeln(R) ,writeln(InitialState).
test21() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,0],1).
test22() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,1],1).
test23() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,2],1).
test24() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,3],1).
test25() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,4],1).
test26() :- get([[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],[2,5],1).

% Validmoves
test1112() :- initBoard(InitialState), validmove(1, InitialState, [2,2]).
test12() :- initBoard2(InitialState), validmove(1,InitialState,[1,1]).

%member
test141() :- member([2,1],[[1,1],[3,1]]).

%set
test15() :- initBoard(InitialState), set(InitialState, NewBoard, [2,1], 2), write(NewBoard).
test16() :- initBoard(InitialState), set(InitialState, NewBoard, [2,1], 2), set(NewBoard, NewBoard2, [2,2], 2), write(NewBoard2).

%findflip
/* outdated tests
test17() :-  findFlipN(1,[[.,.,.,.,.,.],[.,.,.,1,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[3,1],R),write(R).
test18() :-  findFlipS(1,[[.,.,.,.,.,.],[.,.,.,1,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[3,1],R),write(R).
test1337() :- findFlipN(1,[[.,.,.,.,.,.],[.,.,.,1,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]], [3,3], R), writeln(R).
test181() :- initBoard7(InitialState), findFlipSE(2,InitialState,[0,1], R), writeln(R).
*/

%trymove(
test19() :- initBoard3(InitalState), trymove(1,InitalState, [2,0],[0,1]).
test27() :- trymove(1, [[.,.,.,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,2,.,.,.],[.,.,1,.,.,.]],([2,0]),([0,1])).

%Testing -> ; operator.
testcase() :-
	(3=3, 4=4, 5=5 ->
		( 6=6, 7=7 ->
			writeln("huuh?")
			;
			writeln("understandable")
		)
	;
		writeln("didelado")
	).
