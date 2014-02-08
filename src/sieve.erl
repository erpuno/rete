-module(sieve).
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

% S = lists:seq(2,100),
% lists:foldl(fun(A,X) -> X--[A] end,S,
%        [ Y || X <- S,
%               Y <- S,
%               X < math:sqrt(Y)+1,
%               Y rem X==0 ]).

%  integer(X) 1. [2,3,4,5]
%          \
%           integer(Y) 1. [2,3,4,5]
%             /      \
%          X < Y     Y rem X == 0      join
%          1. []          1. [2]       Intersection []
%          2. [3]         2. [2]       Intersection []
%          3. [3,4]       3. [2,4]     Intersection [4]
%          4. [3,4,5]     4. [2,4]     Intersection [4]

sieve(N) -> 
    S=lists:seq(2,N), 
    {T,L} = timer:tc(lists,foldl,[fun(A,X) -> X--[A] end,S,[ Y || X <- S, Y <- S, X < math:sqrt(Y)+1, Y rem X==0 ]]),
    io:format("LC: ~p : ~p~n",[T,length(L)]),
    L.

sieve_ets(N)->
    ets:new(y,[set,named_table,{keypos,1},public]),
  [ ets:insert(y,{X}) || X <- lists:seq(2,N) ],
    H = qlc:q([ets:delete(y,Y) || {X} <- ets:table(y), {Y} <- ets:table(y), X < math:sqrt(Y)+1, Y rem X == 0]),
    {T,R} = timer:tc(qlc,e,[H]),
    L = ets:tab2list(y),
    ets:delete(y),
    io:format("QLC: ~p : ~p~n",[T,length(L)]),
    L.

main() ->
    N = 2000,
    sieve:sieve(N),
    sieve:sieve_ets(N),
    ok.
