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
  search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], StepCounter, NFirstCounter, PathCost) .


start_A_star(InitState, StepCounter, MaxCounter, NFirstCounter, PathCost) :-
  StepCounter =< MaxCounter,
  NewStepCounter is StepCounter + 1,
  start_A_star(InitState, NewStepCounter, MaxCounter, NFirstCounter, PathCost).


start_A_star(_, StepCounter, MaxCounter, _, _) :-
  StepCounter > MaxCounter,
  print("Solution not found").


search_A_star(Queue, ClosedSet, StepCounter, NFirstCounterMax, PathCost) :-
  fetch(Node, Queue, ClosedSet, UserInput, NFirstCounterMax, RestQueue),
  continue(Node, RestQueue, ClosedSet, UserInput, StepCounter, NFirstCounterMax, PathCost).


continue(_, _, _, n, _, _, _).

continue(node(State, Action, Parent, Cost, _), _, ClosedSet, _, _, _,
     path_cost(Path, Cost) ) :-
  goal(State),
  !,
  build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(_, _, _, _, 0, _, _)   :-
  writeln("Licznik wyczerpany"),
  fail.
%read(X),
%X \== y.

continue(Node, RestQueue, ClosedSet, _, StepCounter, NFirstCounterMax, Path)   :-
  StepCounter > 0,
  NewStepCounter is StepCounter - 1,
  expand(Node, NewNodes),
  insert_new_nodes(NewNodes, RestQueue, NewQueue),
  search_A_star(NewQueue, [Node | ClosedSet], NewStepCounter, NFirstCounterMax, Path).


fetch(node(State, Action,Parent, Cost, Score),
[node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet, y, NFirstCounter, RestQueue) :-
  NFirstCounter > 1,
  write("Fetching.. "), writeln(State),
  \+ member(node(State, _, _, _, _) , ClosedSet).
  %print(State), writeln(" zaakceptowany").

fetch(node(State, Action, Parent, Cost, Score),
 [node(State, Action, Parent, Cost, Score)|RestQueue], ClosedSet, UserInput, NFirstCounter, RestQueue) :-
  NFirstCounter == 1,
  \+ member(node(State, _, _, _, _) , ClosedSet),
  writeln("Aktualne stany:"),
  show_states(RestQueue),
  writeln("Czy kontynuowac? y/n"),
  read(UserInput).

fetch(Node, [ _ |RestQueue], ClosedSet, UserInput, NFirstCounter, NewRest) :-
  NFirstCounter > 0,
  NewNFirstCounter is NFirstCounter - 1,
  fetch(Node, RestQueue, ClosedSet, UserInput, NewNFirstCounter, NewRest).

show_states([]) :- writeln("Stan: nil").
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
