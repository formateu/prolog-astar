% etap 2


% kod zmodyfikowany z przykladu studialnego

main :-
  startSample(Path),
  write(Path),
  halt.

/*
testing :-
  goal([pos(0, H)|GRest]),
  findall([pos(0, F)|Rest], (find_neighbour(H, GRest, F, Rest)), Res),
  length(Res, Len),
  writeln(Len),
  halt.
*/

% znajduje zbiorowa roznice miedzy zbiorami rozwiazania
getDiffs(PosTable1, PosTable2, Result) :-
  findall(newpos(Id, Position, NewPosition),
    (member(pos(Id, Position), PosTable1),
     (member(pos(Id, NewPosition), PosTable2)),
     (Position \== NewPosition)
    ),
    Result).

succ([pos(0, EmptyPos)|TilePositions], Diffs, Cost, [pos(0, NewEmptyPos)|NewTilePositions] ) :-
  find_neighbour(EmptyPos, TilePositions, NewEmptyPos, NewTilePositions),
  sum_of_distances([pos(0,EmptyPos)|TilePositions],
                   [pos(0,NewEmptyPos)|NewTilePositions], Cost),
  writeln(Cost),
  getDiffs([pos(0, EmptyPos)|TilePositions], [pos(0, NewEmptyPos)|NewTilePositions], Diffs).


% oryginalny kod z przykladu studialnego
find_neighbour(EmptyPos, [pos(Neighb, NeighbPos)|RestPositions],NeighbPos, [pos(Neighb, EmptyPos)|RestPositions]) :-
  adjacent(EmptyPos, NeighbPos).

%%%% zmieniony kod
find_neighbour(EmptyPos, [T|RestPositions], NewEmptyPos, [T|NewPositions]) :-
  find_neighbour(EmptyPos, RestPositions, NewEmptyPos, NewPositions),
  writeln( EmptyPos/NewEmptyPos).


adjacent(X1/Y1, X2/Y1) :- DiffX is X1-X2,abs(DiffX, 1).
adjacent(X1/Y1, X1/Y2) :- DiffY is Y1-Y2,abs(DiffY, 1).

abs(X, X) :- X >=0, ! .
abs(X, AbsX) :- AbsX  is  -X.


hScore( [ _ |Positions ], HScore)  :-
  goal( [ _ |GoalPositions]),
  sum_of_distances(Positions, GoalPositions, HScore) .

sum_of_distances( [ ], [ ], 0 ) .
sum_of_distances( [pos( _, Pos) | RestPositions],[pos( _, GoalPos) | RestGoalPositions], Sum)  :-
  distance( Pos, GoalPos, Dist),
  sum_of_distances(RestPositions, RestGoalPositions, Sum1),
  Sum  is  Sum1 + Dist.

distance( X1/Y1, X2/Y2, Dist)  :-
  DiffX is X1-X2,
  abs(DiffX, DistX),
  DiffY is Y1-Y2,
  abs(DiffY, DistY),
  Dist is  DistX +  DistY  .

% tablica osemkowa
goal( [ pos(0 , 3/1), pos(1 , 1/3), pos(2 , 2/3),
  pos(3 , 3/3), pos(4 , 1/2), pos(5 , 2/2),
  pos(6 , 3/2), pos(7 , 1/1), pos(8 , 2/1) ] ) .


startSample(PathCost) :-
  start_A_star(
  /*
    [pos(0 , 3/2), pos(1 , 2/3), pos(2 , 1/1)
    , pos(3 , 1/3), pos(4 , 3/1), pos(5 , 1/2)
    , pos(6 , 3/3), pos(7 , 2/1), pos(8 , 2/2) ]
  */
    [ pos(0 , 2/1), pos(1 , 1/3), pos(2 , 2/3),
    pos(3 , 3/3), pos(4 , 1/2), pos(5 , 2/2),
    pos(6 , 3/2), pos(7 , 1/1), pos(8 , 3/1) ]
  , PathCost).


% oryginalny kod z przykladow studialnych

start_A_star(InitState, PathCost) :-
  score(InitState, 0, 0, InitCost, InitScore) ,
  search_A_star([node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost) .


search_A_star(Queue, ClosedSet, PathCost) :-
  fetch(Node, Queue, ClosedSet, RestQueue),
  continue(Node, RestQueue, ClosedSet, PathCost).


continue(node(State, Action, Parent, Cost, _ ) , _, ClosedSet, path_cost(Path, Cost)) :-
  goal(State), ! ,
  build_path(node(Parent, _ , _, _, _) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, Path) :-
  expand(Node, NewNodes),
  insert_new_nodes(NewNodes, RestQueue, NewQueue),
  search_A_star(NewQueue, [Node | ClosedSet], Path).


fetch(node(State, Action,Parent, Cost, Score),
        [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet, RestQueue) :-
    % \+ jest negacja
    \+ member(node(State, _, _, _, _), ClosedSet),  !.

fetch(Node, [ _ |RestQueue], ClosedSet, NewRest) :-
  fetch(Node, RestQueue, ClosedSet, NewRest).


expand(node(State, _, _, Cost, _), NewNodes)  :-
  findall(node(ChildState, Action, State, NewCost, ChildScore) ,
  (succ(State, Action, StepCost, ChildState),
    score(ChildState, Cost, StepCost, NewCost, ChildScore) ) ,
    NewNodes) .


score(State, ParentCost, StepCost, Cost, FScore)  :-
  Cost is ParentCost + StepCost ,
  hScore(State, HScore),
  FScore is Cost + HScore .


insert_new_nodes([], Queue, Queue).

insert_new_nodes([Node|RestNodes], Queue, NewQueue) :-
  insert_p_queue(Node, Queue, Queue1),
  insert_new_nodes(RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [], [Node])  :-    ! .

insert_p_queue(node(State, Action, Parent, Cost, FScore),
  [node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
  [node(State1, Action1, Parent1, Cost1, FScore1)|Rest1])  :-
    FScore >= FScore1,  ! ,
    insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .

insert_p_queue(node(State, Action, Parent, Cost, FScore), Queue,
[node(State, Action, Parent, Cost, FScore)|Queue]).


build_path(node(nil, _, _, _, _), _, Path, Path) :-    ! .

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path)  :-
  del(Nodes, node(EndState, Action, Parent, _, _), Nodes1),
  build_path(node(Parent, _, _, _, _), Nodes1,
    [Action/EndState|PartialPath],Path).


del([X|R],X,R).

del([Y|R],X,[Y|R1]) :-
  X \= Y,
  del(R,X,R1).
