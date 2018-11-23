%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 13:03
%%%-------------------------------------------------------------------
-module(qsort).
-author("kamil").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3, map/2, map2/2, filter/2, digitsNum/1, filter3/0]).

lessThan(List, Arg) -> [X || X <- List , X < Arg].
grtEqThan(List, Arg) -> [X || X <- List , X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomElems(N,Min,Max)-> [X + Min || X <-lists:seq(1, N), X <- [rand:uniform(Max-Min)] ].

compareSpeeds(List, Fun1, Fun2) -> {X, _} = timer:tc(Fun1, [List]), {Y, _} = timer:tc(Fun2, [List]), io:format("Time of Fun1 = ~w ~nTime of Fun2 = ~w ~n", [X, Y]).

map(_, []) -> [];
map(Fun, [Head|Tail]) -> [Fun(Head) | map(Fun, Tail)].

map2(_, []) -> [];
map2(Fun, List) -> [Fun(X) || X <- List].

filter(_,[]) -> [];
filter(Fun, List) -> [X || X <- List, Fun(X) == true].

digitsNum([]) -> [];
digitsNum(N) -> lists:foldl(fun(X, Y) -> X + Y - $0 end, 0, integer_to_list(N)).

%filter3([]) -> [];
filter3() -> [X || X <- randomElems(1000000, 1, 300), X rem 3 == 0 ].