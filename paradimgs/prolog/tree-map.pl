map_build([], nil).
map_build([(Key, Value) | Rest], struct_tree(Key, Value, Left, Right)) :-
    split(Key, Rest, LeftTree, RightTree),
    map_build(LeftTree, Left),
    map_build(RightTree, Right).

split(_, [], [], []).
split(Key, [(K, V) | Rest], [(K, V) | LeftTree], RightTree) :- K < Key, split(Key, Rest, LeftTree, RightTree).
split(Key, [(K, V) | Rest], LeftTree, [(K, V) | RightTree]) :- K >= Key, split(Key, Rest, LeftTree, RightTree).

map_get(struct_tree(Key, Value, _, _), Key, Value).
map_get(struct_tree(NowKey, _, _, Right), Key, Value) :- NowKey < Key, map_get(Right, Key, Value).
map_get(struct_tree(NowKey, _, Left, _), Key, Value) :- NowKey >= Key, map_get(Left, Key, Value).

map_values(nil, []).
map_values(struct_tree(_, Value, Left, Right), Values) :-
    map_values(Left, LeftValues),
    map_values(Right, RightValues),
    append(LeftValues, [Value | RightValues], Values).
