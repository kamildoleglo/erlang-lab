%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 13:16
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("kamil").

%% API
%-export([]).
-export([start/0, init_supervisor/0]).

start() ->
  register(supervisor, spawn(pollution_server_sup, init_supervisor, [])).

init_supervisor() ->
  process_flag(trap_exit, true),
  register(pollutionServer, spawn_link(pollution_server, init, [])),
  loop().

loop() ->
  receive
    {'EXIT', Pid, Reason} ->
      erlang:display("##############################"),
      register(pollutionServer, spawn_link(pollution_server, init, [])),
      loop();
    {request, kill} ->
      pollutionServer ! {request, self(), kill},
      loop()

  end.

