% isil su karakuzu
% 2020400144
% compiling: yes
% complete: no
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .

manhattan_distance(A, B, Distance) :-
    A = [X1, Y1], B = [X2,Y2],
    X12 is abs(X1-X2),
    Y12 is abs(Y1-Y2),
    Distance is X12 + Y12.

% 10 points
% minimum_of_list(+List, -Minimum) :- .

minimum_of_list(List, Minimum) :-
    List = [H|T],
    (min_list(List, H), Minimum is H ; minimum_of_list(T, Minimum)).
    
% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

getByIndex([X], 0, X).
getByIndex([H|_], 0, H).
getByIndex([_|T], I, E) :- NewIndex is I-1, getByIndex(T, NewIndex, E).


find_minimum_distance_type(State, ObjectType, KeyDistance, Key2) :-
    State = [A, O, T],
    InitialX is A.get(x),
    InitialY is A.get(y),
    Type = O.get(Key).get(type),
    Type =@= ObjectType,
    Key2 is Key,
    FinalX = O.get(Key).get(x),
    FinalY = O.get(Key).get(y),
    manhattan_distance([InitialX, InitialY], [FinalX, FinalY], Distance),
    KeyDistance is Distance.

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    State = [A, O, T],
    member(ObjectType, [food, tree, stone, cobblestone, bedrock]),
    findall(KeyDistance, find_minimum_distance_type(State, ObjectType, KeyDistance, Key2), Result),
    minimum_of_list(Result, Minimum),
    Distance is Minimum,
    indexOf(Result, Distance, Index),
    Index2 = Index,
    findall(Key2, find_minimum_distance_type(State, ObjectType, KeyDistance, Key2), Result2),
    getByIndex(Result2, Index2, Key),
    ObjKey is Key,
    get_dict(Key, O, Value),
    Object = Value.

% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .

gen_list(_,0,[]) :-!.
gen_list(I,N,[I|Is]) :- 
         N1 is N-1,
         gen_list(I,N1,Is).

navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    State = [A, O, T],
    InitialX = A.get(x),
    InitialY = A.get(y),
    TestHorizontal = X - InitialX,
    TestVertical = Y - InitialY,
    HorizontalGo is abs(X - InitialX),
    VerticalGo is abs(Y - InitialY),
    (HorizontalGo =:= TestHorizontal, gen_list(go_right, HorizontalGo, List1); gen_list(go_left, HorizontalGo, List1)),
    (VerticalGo =:= TestVertical, gen_list(go_down, VerticalGo, List2); gen_list(go_up, VerticalGo, List2)),
    DepthLimit >= HorizontalGo + VerticalGo,
    append(List1, List2, LastActionList),
    ActionList = LastActionList.

% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
chop_nearest_tree(State, ActionList) :- 
    State = [A, O, T],
    find_nearest_type(State, tree, ObjKey, Object, Distance),
    Object.get(x) = X,
    Object.get(y) = Y,
    navigate_to(State, X, Y, List1, Distance),
    gen_list(left_click_c, 4, List2),
    append(List1, List2, LastActionList),
    ActionList = LastActionList.
    
% 10 points
% mine_nearest_stone(+State, -ActionList) :- .

mine_nearest_stone(State, ActionList) :- 
    State = [A, O, T],
    find_nearest_type(State, stone, ObjKey, Object, Distance),
    Object.get(x) = X,
    Object.get(y) = Y,
    navigate_to(State, X, Y, List1, Distance),
    gen_list(left_click_c, 4, List2),
    append(List1, List2, LastActionList),
    ActionList = LastActionList.
    
% 10 points
% gather_nearest_food(+State, -ActionList) :- .

gather_nearest_food(State, ActionList) :-
    State = [A, O, T],
    find_nearest_type(State, food, ObjKey, Object, Distance),
    Object.get(x) = X,
    Object.get(y) = Y,
    navigate_to(State, X, Y, List1, Distance),
    gen_list(left_click_c, 1, List2),
    append(List1, List2, LastActionList),
    ActionList = LastActionList.

% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
collect_requirements(State, ItemType, ActionList) :- 
    State = [A, O, T],
    item_info(ItemType, Reqs, N),
    L = Reqs.get(log),
    S = Reqs.get(stick),
    C = Reqs.get(cobblestone),
    BL = A.get(inventory).get(log),
    BS = A.get(inventory).get(stick),
    BC = A.get(inventory).get(cobblestone),
    (RL = L - BL, L - BL =:= abs(L - BL); RL = 0),
    (RS = S - BS, S - BS =:= abs(S - BS); RS = 0),
    (RC = C - BC, C - BC =:= abs(C - BC); RC = 0),
    RLS = (2 * RS),
    TotalTreeNeeded = ceiling((RL+RLS)/3),
    TotalStoneNeeded = ceiling(RS / 3).
% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
% 15 points
% make_castle(+State, -ActionList) :- .
