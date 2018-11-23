%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2018 13:03
%%%-------------------------------------------------------------------
-module(pingpong).
-author("kamil").

%% API
-export([start/0, stop/0, play/1, ping_f/0, pong_f/0]).

start() ->
  register(ping, spawn(?MODULE, ping_f, [])),
  register(pong, spawn(?MODULE, pong_f, [])).

stop() ->
  exit(ping, "HALT"),
  exit(pong, "STOP").

play(N) ->
  ping ! {ping, pong, N}.

ping_f() ->

  receive
    {ping, PID, N} -> erlang:display("PONG!"), if N > 0 -> PID ! {pong, self(), N-1}; N =< 0 -> exit("End") end
  after
    20000 -> exit("Idle")
  end,
  ping_f().


pong_f() ->
  receive
    {pong, PID, N} -> erlang:display("PING!"), if N > 0 -> PID ! {ping, self(), N-1}; N =< 0 -> exit("End") end
  after
    20000 -> exit("Idle")
  end,
  pong_f().