-module(lib_misc).
-export([sum/1, qsort/1, pythag/1, perms/1, odds_and_evens/1, odds_and_evens_acc/1]).

sum(L) -> sum(L, 0).

sum([], N) -> N;
sum([H|T], N) -> sum(T, H + N).

%qsort
qsort([]) -> [];
qsort([Pivot|T]) -> 
  qsort([X || X <- T, X < Pivot])
  ++ [Pivot] ++
  qsort([X || X <- T, X >= Pivot]).

% L = [23, 6, 2, 412,7,4,864,14,89,42,68,1, -1 ,56, 0]. // [-1,0,1,2,4,6,7,14,23,42,56,68,89,412,864]

%pythag
pythag(N) ->
  [{A, B, C} ||
    A <- lists:seq(1, N),
    B <- lists:seq(1, N),
    C <- lists:seq(1, N),
    A + B + C =< N,
    A * A + B * B =:= C * C
  ].

%perms
perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L -- [H])].

%> lib_misc:perms("cats").
%["cats","cast","ctas","ctsa","csat","csta","acts","acst",
% "atcs","atsc","asct","astc","tcas","tcsa","tacs","tasc",
% "tsca","tsac","scat","scta","sact","satc","stca","stac"]

%odds_and_evens
odds_and_evens(L) ->
  Odds = [X || X <- L, (X rem 2) =:= 1],
  Evens = [X || X <- L, (X rem 2) =:= 0],
  {Odds, Evens}.

%odds_and_evens_acc
odds_and_evens_acc(L) ->
  odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
case (H rem 2) of
  1 -> odds_and_evens_acc (T, [H|Odds], Evens);
  0 -> odds_and_evens_acc (T, Odds, [H|Evens])
end;
odds_and_evens_acc([], Odds, Evens) ->
  {lists:reverse(Odds), lists:reverse(Evens)}.
  