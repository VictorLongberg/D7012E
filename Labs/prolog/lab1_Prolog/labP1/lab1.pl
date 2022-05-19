%Victor Longberg, Viclon-8

%Three, rooms, R1,R2,R3

%Move, R1 -> R2, Steel key,
%Move, R2 -> R1, Steel key,

%Move R1 -> R3, Brass key,
%Move R3 -> R1, Brass key.

%Brass Key inside R2,
%Steel key inside R1,
%Package inside   R3.

%Only has 2 arms,
    %Drop items.
    %Pick up item

% state(SteelKey,BrassKey,Package,NumberItems,RobotPosition)

%%%%%%%%%%%%%% Walking %%%%%%%%%%%%%%

    %%%%%%%%%%%%%% Move Between R1 and R3 %%%%%%%%%%%%%%

%Move to R3 from R1
move(state(SK, has, Pa, Items, r1),
walk(r1, r3),
state(SK, has, Pa, Items, r3)).

%Move to R1 from R3
move(state(SK, has, Pa, Items, r3),
walk(r3, r1),
state(SK, has, Pa, Items, r1)).

    %%%%%%%%%%%%%% Move Between R1 and R2 %%%%%%%%%%%%%%

%Move to R2 from R1
move(state(has, BK, Pa, Items, r1),
walk(r1, r2),
state(has, BK, Pa, Items, r2)).

%Move to R1 from R2
move(state(has, BK, Pa, Items, r2), % Kan plocka brass key här men är inte relevant 
walk(r2, r1),                    % hur man kan röra på sig.
state(has, BK, Pa, Items, r1)).
%NextState) :- Items =< 1, NextState = state(has,Bk,Pa,Items,r1).

%%%%%%%%%%%%%% Grasp Item %%%%%%%%%%%%%%

% Grasp Steel Key.
move(state(Room,BK,Pa,Items,Room),
grasp(steelKey,Room),
state(has,BK,Pa,Items2,Room)) :- Items < 2, Items2 is Items + 1.

% Grasp Brass Key.
move(state(SK,Room,Pa,Items,Room),
grasp(brassKey,Room),
state(SK,has,Pa,Items2,Room)) :- Items < 2, Items2 is Items + 1.

%Grasp Package
move(state(SK,BK,Room,Items,Room),
grasp(package,Room),
state(SK,BK,has,Items2,Room)) :- Items < 2, Items2 is Items + 1.

%%%%%%%%%%%%%%% Drop Item %%%%%%%%%%%%%%

%Drop Steel Key.
move(state(has,Bk,Pa,Items,Room),
drop(steelKey,Room),
state(Room,Bk,Pa,Items2,Room)) :- Items2 is Items -1. %Cant go below zero, since we require a item in order to drop a item.

%Drop Brass Key.
move(state(SK, has, Pa, Items, Room),
drop(brassKey,Room),
state(SK, Room, Pa, Items2, Room)) :- Items2 is Items -1. %Cant go below zero, since we require a item in order to drop a item.

%Drop Brass Key.
move(state(SK, BK, has, Items, Room),
drop(package,Room),                                       %Im saying the item is dropped inside the room of which i am stadning in.
state(SK, BK, Room, Items2, Room)) :- Items2 is Items -1. %Cant go below zero, since we require a item in order to drop a item.

%%%%%%%%%%%%%%% Solve and Print %%%%%%%%%%%%%%%

%solve_robot(state(_,_,has,_,r2), _, [done| []]). om de är så att roboten bara ska in
solve_robot(state(_,_,r2,_,r2), _, [done| []]).
solve_robot(State1, N, [Move|Trace2]) :- %addar varje move till listan av mitt "trace" dvs path
    N > 0,
	move(State1, Move, State2),
	N2 is N - 1,
	solve_robot(State2, N2, Trace2).


start_solve_robot(Trace) :- solve_robot(state(r1,r2,r3,0,r1),13,Trace).
