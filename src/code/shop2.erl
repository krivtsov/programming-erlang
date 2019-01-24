-module(shop2).
-export([total/1, total2/1]).
-import(lists, [map/2, sum/1]).

total(L) -> sum(map(fun({What, N}) -> shop:cost(What) * N end, L)).

total2(L) -> sum([shop:cost(What) * N || {What, N} <- L]).

% L = [{oranges, 4}, {newspaper,1}, {apples, 10}, {pears, 6},{milk, 3}].