%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2018 20:11
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kamil").

%% API
-export([start/0, stop/0, getAirQualityIndex/2, getDailyMean/2, getStationMean/2, getOneValue/3, findStation/1, removeValue/3, addValue/4, addStation/2]).
-export([init/0]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

init() ->
  {monitor, Monitor} = pollution:createMonitor(),
  loop(Monitor).


parse_response({error, ErrorText}, Monitor, Pid) ->
  Pid ! {reply, ErrorText},
  Monitor;

parse_response({ok, OkText}, Monitor, Pid) ->
  Pid ! {reply, OkText},
  Monitor;

parse_response({monitor, NewMonitor}, _, Pid) ->
  Pid ! {reply, ok},
  NewMonitor;

parse_response(Error, Monitor, Pid) ->
  Pid ! {reply, Error},
  Monitor.


loop(Monitor) ->
  receive
    {request, Pid, {addStation, Name, {X, Y}}} ->
      Response = pollution:addStation(Name, {X, Y}, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {addValue, Name, Date, Type, Value}} ->
      Response = pollution:addValue(Name, Date, Type, Value, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {removeValue, Name, Date, Type}} ->
      Response = pollution:removeValue(Name, Date, Type, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {findStation, Name}} ->
      Response = pollution:findStation(Name, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {getOneValue, Type, Date, StationID}} ->
      Response = pollution:getOneValue(Type, Date, StationID, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {getStationMean, Type, StationID}} ->
      Response = pollution:getStationMean(Type, StationID, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {getDailyMean, Type, Day}} ->
      Response = pollution:getDailyMean(Type, Day, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, {getAirQualityIndex, StationID, Date}} ->
      Response = pollution:getAirQualityIndex(StationID, Date, Monitor),
      loop(parse_response(Response, Monitor, Pid));

    {request, Pid, stop} ->
      Pid ! {reply, ok};

    {request, Pid, _} ->
      Pid ! {error, "Bad command"},
      loop(Monitor)

end.

call(Message) ->
  pollutionServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.


addStation(Name, {X, Y}) ->
  call({addStation, Name, {X, Y}}).

addValue(Name, Date, Type, Value) ->
  call({addValue, Name, Date, Type, Value}).

removeValue(Name, Date, Type) ->
  call({removeValue, Name, Date, Type}).

findStation(Name) ->
  call({findStation, Name}).

getOneValue(Type, Date, StationID) ->
  call({getOneValue, Type, Date, StationID}).

getStationMean(Type, StationID) ->
  call({getStationMean, Type, StationID}).

getDailyMean(Type, Day) ->
  call({getDailyMean, Type, Day}).

getAirQualityIndex(StationID, Date) ->
  call({getAirQualityIndex, StationID, Date}).

stop() ->
  call(stop).