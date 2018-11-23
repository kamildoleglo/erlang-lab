%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, Kamil Doległo
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 14:13
%%%-------------------------------------------------------------------
-module(pollution).
-author("kamil").

%% API
-export([createMonitor/0,
        addStation/3,
        addValue/5,
        removeValue/4,
        getOneValue/4,
        findStation/2,
        getStationMean/3,
        getDailyMean/3,
        getAirQualityIndex/3]).

-record(monitor, {stations = #{}, locationsNames = #{}}).
-record(station, {name = "", coordinates = {}, measurement_types = #{}}).
-record(measurement_type, {measurements = #{}}).

% #monitor {name => #station}
% #station {type => #measurement_type}
% #measurement_type {date => value}

createMonitor() -> {monitor, #monitor{}}.

addStation(Name, {X, Y}, Monitor) ->
  addStation(Name, {X, Y}, Monitor, maps:find({X,Y}, Monitor#monitor.locationsNames), maps:find(Name, Monitor#monitor.stations));
addStation(_,_,_) ->
  {error, "Cannot add station"}.

addStation(Name, {X, Y}, Monitor, error, error) ->
  S = Monitor#monitor.stations,
  L = Monitor#monitor.locationsNames,
  Monitor_new = Monitor#monitor{stations = maps:put(Name, #station{name = Name, coordinates = {X,Y}}, S),
    locationsNames = maps:put({X, Y}, Name, L)},
  {monitor, Monitor_new};

addStation(_, _, _, _, _) ->
  {error, "Station already exists"}.



addValue(Name, Date, Type, Value, Monitor) when is_list(Name) ->
  addValue(Name, Date, Type, Value, Monitor, maps:find(Name, Monitor#monitor.stations));

addValue(Location, Date, Type, Value, Monitor) when is_tuple(Location) ->
  Name = maps:find(Location, Monitor#monitor.locationsNames),
  case Name of
    {ok, NameVal} ->
      addValue(NameVal, Date, Type, Value, Monitor, maps:find(NameVal, Monitor#monitor.stations));
    _ ->
      {error, "Cannot find station"}
  end.

addValue(Name, Date, Type, Value, Monitor, {ok, Station}) ->  %if we found station then find measurement type
  addValue(Name, Date, Type, Value, Monitor, Station, maps:find(Type, Station#station.measurement_types));

addValue(_, _, _, _, _, error) ->  %if we cannot find station raise error
  {error, "Cannot find station"}.

addValue(Name, Date, Type, Value, Monitor, Station, {ok, Measurement_type}) -> %if measurement type has been found then check if we have a measurement already
  addValue(Name, Date, Type, Value, Monitor, Station, Measurement_type, maps:find(Date, Measurement_type#measurement_type.measurements));

addValue(Name, Date, Type, Value, Monitor, Station, error) -> %if we haven't found measurement type though, add one and go back
  Station_new = Station#station{measurement_types = maps:put(Type, #measurement_type{}, Station#station.measurement_types)},
  addValue(Name, Date, Type, Value, Monitor, {ok, Station_new}).

addValue(Name, Date, Type, Value, Monitor, Station, Measurement_type, error) -> %when there is no measurement with given type for given date, then add one
  Measurement_type_new = Measurement_type#measurement_type{measurements = maps:put(Date, Value, Measurement_type#measurement_type.measurements)},
  Station_new = Station#station{measurement_types = maps:update(Type, Measurement_type_new, Station#station.measurement_types)},
  Monitor_new = Monitor#monitor{stations = maps:update(Name, Station_new, Monitor#monitor.stations)},
  {monitor, Monitor_new};

addValue(_, _, _, _, _, _, _, _) ->
  {error, "Cannot add measurement"}.


removeValue(Name, Date, Type, Monitor) when is_list(Name) ->
  removeValue(Name, Date, Type, Monitor, maps:find(Name, Monitor#monitor.stations));

removeValue(Location, Date, Type, Monitor) when is_tuple(Location) ->
  Name = maps:find(Location, Monitor#monitor.locationsNames),
  case Name of
    {ok, NameVal} ->
      removeValue(NameVal, Date, Type, Monitor, maps:find(NameVal, Monitor#monitor.stations));
    _ ->
      {error, "Cannot find station"}
  end.

removeValue(Name, Date, Type, Monitor, {ok, Station}) ->  %if we found station then find measurement type
  removeValue(Name, Date, Type, Monitor, Station, maps:find(Type, Station#station.measurement_types));

removeValue(_, _, _, _, Error) ->  %if we cannot find station, return error
  Error.

removeValue(Name, Date, Type, Monitor, Station, {ok, Measurement_type}) -> %if measurement type has been found then delete measurement
  Measurement_type_new = Measurement_type#measurement_type{measurements = maps:remove(Date, Measurement_type#measurement_type.measurements)},
  Station_new = Station#station{measurement_types = maps:update(Type, Measurement_type_new, Station#station.measurement_types)},
  Monitor_new = Monitor#monitor{stations = maps:update(Name, Station_new, Monitor#monitor.stations)},
  {monitor, Monitor_new};

removeValue(_, _, _, _, _, _) ->
  {error, "Cannot remove measurement "}.


findStation(Name, Monitor) when is_list(Name) ->
  case maps:find(Name, Monitor#monitor.stations) of
    {ok, Station} ->
      {ok, Station};
    _ ->
      {error, "Cannot find station"}
  end;

findStation(Location, Monitor) when is_tuple(Location) ->
  case maps:find(Location, Monitor#monitor.locationsNames) of
    {ok, Name} ->
      findStation(Name, Monitor);
    _ ->
      {error, "Cannot find station"}
  end.

getOneValue(Type, Date, StationID, Monitor) ->
  getOneValue(Type, Date, StationID, Monitor, findStation(StationID, Monitor)).

getOneValue(Type, Date, StationID, Monitor, {ok, Station}) ->
  getOneValue(Type, Date, StationID, Monitor, Station, maps:find(Type, Station#station.measurement_types));

getOneValue(_, _, _, _, Error) ->
  Error.

getOneValue(_, Date, _, _, _, {ok, Measurement_type}) ->
  case maps:find(Date, Measurement_type#measurement_type.measurements) of
    {ok, Value} ->
      {ok, Value};
    _ ->
      {error, "Cannot find measurement"}
  end;

getOneValue(_, _, _, _, _, error) ->
  {error, "Cannot find measurement type"}.


getStationMean(Type, StationID, Monitor) ->
  getStationMean(Type, StationID, Monitor, findStation(StationID, Monitor)).

getStationMean(Type, StationID, Monitor, {ok, Station}) ->
  getStationMean(Type, StationID, Monitor, Station, maps:find(Type, Station#station.measurement_types));

getStationMean(_, _, _, Error) ->
  Error.

getStationMean(_, _, _, _, {ok, Measurement_type}) ->
  Values = maps:values(Measurement_type#measurement_type.measurements),
  {ok, lists:sum(Values) / length(Values)};

getStationMean(_, _, _, _, error) ->
  {error, "Cannot find measurement type"}.


getDailyMean(Type, Day, Monitor) ->
  Stations = maps:values(Monitor#monitor.stations),
  Sums = lists:map(fun(Station) -> getDailyMean(Type, Day, Monitor, Station) end, Stations),
  Num = lists:foldl(fun({S, _}, Sum) -> S + Sum end, 0, Sums),
  Denom = lists:foldl(fun({_, L}, Sum) -> L + Sum end, 0, Sums),
  {ok, divide(Num, Denom)}.

getDailyMean(Type, Day, Monitor, Station) ->
  getDailyMean(Type, Day, Monitor, Station, maps:find(Type, Station#station.measurement_types)).

getDailyMean(_, Day, _, _, {ok, Measurement_type}) ->
  Measurements = maps:filter(fun(K, _) -> datetime_to_date(K) == Day end, Measurement_type#measurement_type.measurements),
  Measurements_values = maps:values(Measurements),
  {lists:sum(Measurements_values), length(Measurements_values)};

getDailyMean(_, _, _, _, error) ->
  {error, "Cannot find measurement type"}.


datetime_diff(DateTime1, Datetime2) ->
  abs(calendar:datetime_to_gregorian_seconds(DateTime1) -
  calendar:datetime_to_gregorian_seconds(Datetime2)).

datetime_to_date(DateTime) -> {Date, {_, _, _}} = DateTime, Date.


getAirQualityIndex(StationID, Date, Monitor) ->
  getAirQualityIndex(StationID, Date, Monitor, findStation(StationID, Monitor)).

getAirQualityIndex(_, {Date, {Hour, _, _}}, _, {ok, Station}) ->
  Measurement_types = maps:to_list(Station#station.measurement_types),
  List = lists:map(fun({K, V}) -> case getNorm(K) of
                                    error -> {0, V};
                                    Val -> {Val, maps:filter(fun(D, _) -> datetime_diff({Date, {Hour, 30, 0}}, D) < 1800 end, V#measurement_type.measurements)}
                                    end
                   end, Measurement_types),
  Max = lists:max(lists:map(fun({K,V}) -> divide(lists:max(maps:values(V)), K) * 100 end, List)),
  {ok, Max};

getAirQualityIndex(_, _, _, Error) ->
  Error.

divide(A, B) when B /= 0 ->
  A/B;
divide(_, _) ->
  0.

getNorm(Type) ->
  LType = string:lowercase(Type),
  case LType of
    "pm10" -> 50.0;
    "pm2,5" -> 30.0;
    "no2" -> 40.0;
    "o3" -> 50.0;
    "co" -> 1.0;
    "co2" -> 40.0;
    "nh3" -> 200.0;
    "pb" -> 0.5;
    _ -> 0
  end.


%+createMonitor/0 - tworzy i zwraca nowy monitor zanieczyszczeń;
%~addStation/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
%~+addValue/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
%+removeValue/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
%+getOneValue/4 - zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
%+getStationMean/3 - zwraca średnią wartość parametru danego typu z zadanej stacji;
%~getDailyMean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;
%+getAirQualityIndex/3