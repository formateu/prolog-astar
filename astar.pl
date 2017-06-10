% dane wejsciowe ze slajdu Parewicza

% sciezka ab, z a do b i koszt 2
succ(a,ab,2,b).
succ(b,bf,3,f).
succ(a,ac,3,c).
succ(b,bg,4,g).
succ(g,gm,2,m).
succ(c,cd,2,d).
succ(d,dm,2,m).
succ(c,ce,3,e).
succ(e,em,5,m).

% punkt docelowy
goal(m).

% wartosci f heurystycznej dla danego punktu
hScore(a,4).
hScore(b,4).
hScore(f,7).
hScore(g,1).
hScore(m,0).
hScore(c,3).
hScore(d,1).
hScore(e,4).


startSample(StepCounter, MaxCounter, NFirstCounter, PathCost) :-
  start_A_star(a, StepCounter, MaxCounter, NFirstCounter, PathCost).

% program
% StepCounter - licznik zaglebien
start_A_star(InitState, StepCounter, MaxCounter, NFirstCounter, PathCost) :-
  StepCounter =< MaxCounter,
  score(InitState, 0, 0, InitCost, InitScore) ,
  writeln(StepCounter / MaxCounter),
  search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], StepCounter, NFirstCounter, NFirstCounter, PathCost) .


start_A_star(InitState, StepCounter, MaxCounter, NFirstCounter, PathCost) :-
  StepCounter =< MaxCounter,
  NewStepCounter is StepCounter + 1,
  start_A_star(InitState, NewStepCounter, MaxCounter, NFirstCounter, PathCost).


start_A_star( InitState, StepCounter, MaxCounter, NFirstCounter, PathCost) :-
  StepCounter > MaxCounter,
  print("Solution not found").


search_A_star(Queue, ClosedSet, StepCounter, NFirstCounter, NFirstCounterMax, PathCost) :-
  fetch(Node, Queue, ClosedSet,NFirstCounter, RestQueue),
  continue(Node, RestQueue, ClosedSet, StepCounter, NFirstCounterMax, PathCost).


continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, StepCounter, NFirstCounterMax,
path_cost(Path, Cost) ) :-
  goal( State),
  !,
  build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, NFirstCounterMax, Path)   :-
  StepCounter == 0,
  writeln("Licznik wyczerpany"),
  1 == 0.
%read(X),
%X \== y.

continue(Node, RestQueue, ClosedSet, StepCounter, NFirstCounterMax, Path)   :-
  StepCounter > 0,
  NewStepCounter is StepCounter - 1,
  expand(Node, NewNodes),
  insert_new_nodes(NewNodes, RestQueue, NewQueue),
  search_A_star(NewQueue, [Node | ClosedSet ], NewStepCounter, NFirstCounterMax, NFirstCounterMax, Path).


fetch(node(State, Action,Parent, Cost, Score),
[node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet, NFirstCounter, RestQueue) :-
  % \+ jest negacja
  \+ member(node(State, _, _, _, _) , ClosedSet).
  %print(State), writeln(" zaakceptowany").

fetch(Node, [ _ |RestQueue], ClosedSet, NFirstCounter,  NewRest) :-
  NFirstCounter > 0,
  NewNFirstCounter is NFirstCounter - 1,
  fetch(Node, RestQueue, ClosedSet, NewNFirstCounter, NewRest).

fetch(Node, RestQueue, [Node|ClosedSet], NFirstCounter, NewRest) :-
  NFirstCounter > 0,
  NewNFirstCounter is NFirstCounter - 1,
  print("Aktualne wezly: "), read(Choice), !,
  show_states(RestQueue),
  member(node(Choice, _, _, _, _), RestQueue),
  nth0(Pos, RestQueue, node(Choice, _, _, _, _)),
  nth0(Pos, RestQueue, Chosen),
  writeln(Chosen),
  delete(RestQueue, Chosen, NewRestQueue),
  fetch(Chosen, NewRestQueue, ClosedSet, NewNFirstCounter, NewRestQueue).

show_states([]).
show_states([node(State, _, _, _, Score)|Rest]) :-
  format('Stan: ~w\tOcena: ~w\n', [State, Score]),
  show_states(Rest).

expand(node(State, _, _, Cost, _), NewNodes)  :-
  print("expanding.. "), %writeln(State),
  findall(node(ChildState, Action, State, NewCost, ChildScore) ,
  (succ(State, Action, StepCost, ChildState),
    score(ChildState, Cost, StepCost, NewCost, ChildScore) ) ,
    NewNodes) .


score(State, ParentCost, StepCost, Cost, FScore)  :-
  Cost is ParentCost + StepCost ,
  hScore(State, HScore),
  FScore is Cost + HScore.


insert_new_nodes([], Queue, Queue).

insert_new_nodes([Node|RestNodes], Queue, NewQueue) :-
  insert_p_queue(Node, Queue, Queue1),
  insert_new_nodes(RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [], [Node])   :-    ! .

insert_p_queue(node(State, Action, Parent, Cost, FScore),
[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1])  :-
  FScore >= FScore1,  ! ,
  insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .

insert_p_queue(node(State, Action, Parent, Cost, FScore), Queue,
[node(State, Action, Parent, Cost, FScore)|Queue]).


build_path(node(nil, _, _, _, _ ), _, Path, Path) :-    ! .

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path)  :-
  del(Nodes, node(EndState, Action, Parent, _, _), Nodes1),
  build_path(node(Parent, _, _, _, _), Nodes1,
  [Action/EndState|PartialPath],Path).


del([X|R],X,R).

del([Y|R],X,[Y|R1]) :-
  X \= Y,
  del(R,X,R1).
