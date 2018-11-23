%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, Kamil Doległo
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 14:13
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("kamil").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-record(monitor, {stations = #{}, locationsNames = #{}}).
-record(station, {name = "", coordinates = {}, measurement_types = #{}}).
-record(measurement_type, {measurements = #{}}).

% #monitor {name => #station}
% #station {type => #measurement_type}
% #measurement_type {date => value}

start_stop_test() ->
  ?assertMatch(true, pollution_server:start()),
  ?assertMatch(ok, pollution_server:stop()).

add_station_test() ->
  pollution_server:start(),

  ?assertMatch(ok, pollution_server:addStation("A", {0, 1})),
  ?assertMatch(ok, pollution_server:addStation("B", {2, 3})),

  ?assertMatch("Station already exists", pollution_server:addStation("A", {0, 1})),
  pollution_server:stop().

add_value_test() ->
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  Time = calendar:local_time(),

  ?assertMatch(ok, pollution_server:addValue({0, 1}, Time, "PM10", 100)),

  ?assertMatch("Cannot add measurement", pollution_server:addValue({0, 1}, Time, "PM10", 72)),

  ?assertMatch("Cannot find station",
    pollution_server:addValue({-1000, 0}, calendar:local_time(), "Temperature", 21.5)),

  pollution_server:stop().




remove_value_test() ->
  Time = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  pollution_server:addValue({0, 1}, Time, "PM10", 100),

  ?assertMatch(ok, pollution_server:removeValue("A", Time, "PM10")),

  pollution_server:stop().

get_one_value_test() ->
  Time = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  pollution_server:addValue({0, 1}, Time, "PM10", 100),

  ?assertMatch(100, pollution_server:getOneValue("PM10", Time, "A")),

  ?assertMatch("Cannot find measurement type",
    pollution_server:getOneValue("PM25", calendar:local_time(), "A")),

  ?assertMatch("Cannot find measurement",
    pollution_server:getOneValue("PM10", calendar:universal_time(), "A")),

  pollution_server:stop().


get_station_mean_test() ->
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  {Date, {Hour, Minute, Second}} = calendar:local_time(),

  pollution_server:addValue({0, 1}, {Date, {Hour, Minute, Second}}, "PM10", 100),
  pollution_server:addValue("A", {Date, {(Hour + 1) rem 24, Minute, Second}}, "PM10", 50),
  pollution_server:addValue({0, 1}, {Date, {(Hour + 2) rem 24, Minute, Second}}, "PM2,5", 10),

  ?assertMatch(75.0, pollution_server:getStationMean("PM10", "A")),
  pollution_server:stop().


get_daily_mean_test() ->
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  pollution_server:addStation("B", {2, 3}),
  Time = {Date, {Hour, Minute, Second}} = calendar:local_time(),

  pollution_server:addValue("A", Time, "PM10", 80),
  pollution_server:addValue("A", {Date, {(Hour + 1) rem 24, Minute, Second}}, "PM10", 120),
  pollution_server:addValue("B", {Date, {(Hour + 2) rem 24, Minute, Second}}, "PM10", 40),

  ?assertMatch(80.0, pollution_server:getDailyMean("PM10", Date)),

  pollution_server:stop().

get_air_quality_test() ->
  pollution_server:start(),
  pollution_server:addStation("A", {0, 1}),
  Time = {Date, {Hour, Minute, Second}} = calendar:local_time(),

  pollution_server:addValue("A", Time, "PM10", 80),
  pollution_server:addValue("A", {Date, {Hour, (Minute + 10) rem 60, Second}}, "PM10", 120),
  pollution_server:addValue("A", {Date, {Hour, Minute, Second}}, "PM25", 40),

  ?assertMatch(240.0, pollution_server:getAirQualityIndex("A", Time)),

  pollution_server:stop().

